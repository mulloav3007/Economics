#!/usr/bin/env python3
"""Descubre y descarga los tabulados sectoriales del INE sin URLs mensuales fijas.

Ejecutar desde la raíz del repositorio antes de R/imacec_data.R.
Dependencias: requests, beautifulsoup4, playwright (Chrome solo como respaldo).
Este módulo NO estima ni modifica el nowcast. Su salida es un Excel validado por
firma binaria y un manifiesto; la validez de las series se comprueba en R.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import logging
import os
import re
import tempfile
import unicodedata
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from urllib.parse import unquote, urljoin, urlparse

import requests
from bs4 import BeautifulSoup
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry

LOG = logging.getLogger("ine_releases")
BASE = "https://www.ine.gob.cl"
MONTHS = {
    "enero": 1, "febrero": 2, "marzo": 3, "abril": 4,
    "mayo": 5, "junio": 6, "julio": 7, "agosto": 8,
    "septiembre": 9, "setiembre": 9, "octubre": 10,
    "noviembre": 11, "diciembre": 12,
}
PAGES = {
    "ipi": "/estadisticas-por-tema/industria-energia-y-construccion/indice-de-produccion-industrial",
    "comercio": "/estadisticas-por-tema/comercio-y-servicios/actividad-mensual-del-comercio",
    "ivs": "/estadisticas-por-tema/comercio-y-servicios/ventas-mensuales-de-servicios",
}
DESTINATIONS = {
    "ipi": os.getenv("IMACEC_IPI_FILE", "data/raw/ine_ipi_current.xls"),
    "comercio": os.getenv("IMACEC_COMMERCE_FILE", "data/raw/ine_commerce_current.xls"),
    "ivs": os.getenv("IMACEC_IVS_FILE", "data/raw/series_mensuales_desde_enero_2018_a_la_fecha.xls"),
}
URL_HINTS = {
    "ipi": ("indice-de-produccion-industrial", "series-empalmadas-y-mensuales"),
    "comercio": ("actividad-mensual-del-comercio", "series-mensuales"),
    "ivs": ("ventas-de-servicios", "series_mensuales"),
}
MONTH_PATTERN = re.compile(r"(?:\ba[\s_-]+|\bde[\s_-]+)(" + "|".join(MONTHS) + r")[\s_-]+(?:de[\s_-]+)?(20\d{2})\b")
EXCEL_OLE = bytes.fromhex("d0cf11e0a1b11ae1")


def normalized(value: str) -> str:
    value = unicodedata.normalize("NFKD", unquote(value or ""))
    return "".join(ch for ch in value if not unicodedata.combining(ch)).lower()


def period_in(text: str) -> str | None:
    m = MONTH_PATTERN.search(normalized(text))
    return f"{int(m.group(2)):04d}-{MONTHS[m.group(1)]:02d}" if m else None


def official_link(url: str) -> bool:
    parsed = urlparse(url)
    return parsed.scheme == "https" and (parsed.hostname == "ine.gob.cl" or
        (parsed.hostname or "").endswith(".ine.gob.cl")) and "/docs/" in parsed.path.lower()


@dataclass(frozen=True)
class Candidate:
    url: str
    title: str
    period: str | None
    score: int


def score_candidate(product: str, url: str, title: str) -> Candidate | None:
    if not official_link(url):
        return None
    s = normalized(unquote(url))
    t = normalized(title)
    context = f"{s} {t}"
    directory, series_hint = URL_HINTS[product]
    if directory not in s:
        return None
    if product == "ipi":
        if "base-promedio-2018-100" not in s and "base-promedio-2018_100" not in s:
            return None
        if not ("series empalmadas" in t or series_hint in s):
            return None
        period = period_in(context)
        if period is None:
            return None
        score = int(period.replace("-", "")) * 100
    else:
        if "serie" not in context or not (series_hint in s or "series mensuales" in t):
            return None
        period = None  # Un archivo «desde enero de 2018 a la fecha» no fecha el último dato.
        score = 20000000
    score += 60 if "2018" in s else 0
    score += 15 if re.search(r"\.xlsx?(?:\?|$)", s) else 0
    score += 5 if "serie" in t else 0
    return Candidate(url=url, title=title.strip()[:180], period=period, score=score)


def extract_candidates(html: str, page_url: str, product: str) -> list[Candidate]:
    """Lee enlaces reales renderizados o incrustados en el HTML, sin inventar URL."""
    soup = BeautifulSoup(html, "html.parser")
    hits: dict[str, Candidate] = {}
    for anchor in soup.select("a[href], a[data-href], a[data-url]"):
        link = anchor.get("href") or anchor.get("data-href") or anchor.get("data-url")
        if not link or link.startswith(("javascript:", "#")):
            continue
        text = anchor.get_text(" ", strip=True)
        parent = anchor.parent
        context = (text + " " + (parent.get_text(" ", strip=True) if parent else ""))[:350]
        url = urljoin(page_url, link.replace("&amp;", "&"))
        c = score_candidate(product, url, context)
        if c and (url not in hits or c.score > hits[url].score):
            hits[url] = c
    # Algunos CMS serializan URLs en scripts JSON, sin un enlace <a> visible.
    regex = re.compile(r"(?:https?:)?//[^\s\"'<>]+?\.xlsx?(?:\?[^\s\"'<>]*)?|/docs/default-source/[^\s\"'<>]+?\.xlsx?(?:\?[^\s\"'<>]*)?", re.I)
    unescaped = html.replace("\\/", "/").replace("\\u0026", "&").replace("&amp;", "&")
    for match in regex.finditer(unescaped):
        raw = match.group(0).rstrip(",);]")
        url = urljoin(page_url, raw)
        context = unescaped[max(0, match.start()-180):match.end()+50]
        c = score_candidate(product, url, context)
        if c and (url not in hits or c.score > hits[url].score):
            hits[url] = c
    return sorted(hits.values(), key=lambda c: (c.score, c.url), reverse=True)


def make_session() -> requests.Session:
    session = requests.Session()
    retry = Retry(total=3, backoff_factor=1.0, status_forcelist=[429, 500, 502, 503, 504])
    session.mount("https://", HTTPAdapter(max_retries=retry))
    session.headers.update({
        "User-Agent": "Economics-IMACEC-INE/1.0 (+public statistics)",
        "Accept": "text/html,application/vnd.ms-excel,application/vnd.openxmlformats-officedocument.spreadsheetml.sheet,*/*",
    })
    return session


def browse_candidates(page_url: str, product: str) -> list[Candidate]:
    """Plan B: reproduce los clicks de Cuadros estadísticos del sitio del INE."""
    from playwright.sync_api import sync_playwright

    with sync_playwright() as playwright:
        browser = playwright.chromium.launch(channel="chrome", headless=True,
            args=["--disable-dev-shm-usage"])
        try:
            page = browser.new_page(accept_downloads=False, locale="es-CL")
            page.goto(page_url, wait_until="domcontentloaded", timeout=60000)
            for action in ("cuadros", "base"):
                selector = (r"cuadros estadisticos|cuadros estadísticos" if action == "cuadros"
                            else r"base promedio (?:a[nñ]o )?2018\s*[=]_?\s*100")
                nodes = page.get_by_text(re.compile(selector, re.I))
                for i in range(min(nodes.count(), 12)):
                    node = nodes.nth(i)
                    try:
                        if node.is_visible(timeout=1000):
                            node.click(timeout=3500)
                            page.wait_for_timeout(500)
                            break
                    except Exception:
                        continue
            page.wait_for_timeout(2500)
            found = extract_candidates(page.content(), page.url, product)
            if not found:
                page.wait_for_timeout(2500)
                found = extract_candidates(page.content(), page.url, product)
            return found
        finally:
            browser.close()


def discover(session: requests.Session, product: str) -> tuple[list[Candidate], str]:
    url = urljoin(BASE, PAGES[product])
    direct: list[Candidate] = []
    try:
        resp = session.get(url, timeout=(12, 40))
        resp.raise_for_status()
        direct = extract_candidates(resp.text, resp.url, product)
    except Exception as exc:
        LOG.warning("HTML directo %s: %s", product, exc)
    if direct:
        return direct, "html"
    try:
        rendered = browse_candidates(url, product)
        if rendered:
            return rendered, "browser"
    except Exception as exc:
        LOG.warning("Navegador %s: %s", product, exc)
    return [], "no_discovery"


def file_kind(content: bytes) -> str:
    if content.startswith(EXCEL_OLE):
        return ".xls"
    if content.startswith(b"PK\x03\x04"):
        return ".xlsx"
    raise ValueError("El servidor no devolvió un libro Excel XLS/XLSX (firma inválida)")


def download(session: requests.Session, candidate: Candidate, output: str) -> dict:
    with session.get(candidate.url, timeout=(12, 75), stream=True) as response:
        response.raise_for_status()
        if not official_link(response.url):
            raise ValueError("Redirección fuera del dominio oficial del INE")
        chunks, size = [], 0
        for chunk in response.iter_content(65536):
            size += len(chunk)
            if size > 40 * 1024 * 1024:
                raise ValueError("El Excel excede el límite de 40 MB")
            chunks.append(chunk)
    content = b"".join(chunks)
    if len(content) < 5000:
        raise ValueError("Excel demasiado pequeño")
    ext = file_kind(content)
    dest = Path(output).with_suffix(ext)
    dest.parent.mkdir(parents=True, exist_ok=True)
    handle, tmp = tempfile.mkstemp(dir=dest.parent, prefix=".ine-", suffix=ext)
    try:
        with os.fdopen(handle, "wb") as fh:
            fh.write(content)
        os.replace(tmp, dest)
    finally:
        if os.path.exists(tmp):
            os.unlink(tmp)
    return {
        "path": dest.as_posix(), "url": response.url, "label": candidate.title,
        "period": candidate.period, "size": len(content),
        "sha256": hashlib.sha256(content).hexdigest(),
        "md5": hashlib.md5(content).hexdigest(),  # Para validación base R
    }


def run(products: list[str], manifest: Path, strict: bool = False) -> int:
    session = make_session()
    result: dict = {
        "retrieved_at_utc": datetime.now(timezone.utc).isoformat(),
        "products": {}, "failures": {},
    }
    for product in products:
        candidates, strategy = discover(session, product)
        failure = "No se detectaron enlaces oficiales a las series Excel"
        for item in candidates[:12]:
            try:
                entry = download(session, item, DESTINATIONS[product])
                entry["discovery"] = strategy
                result["products"][product] = entry
                LOG.info("%s: %s %s (%s)", product, entry["period"], entry["path"], strategy)
                break
            except Exception as exc:
                failure = f"{item.url}: {exc}"
                LOG.warning("Descarga %s: %s", product, failure)
        if product not in result["products"]:
            result["failures"][product] = failure
            LOG.error("%s sin descarga validada: %s", product, failure)
    manifest.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(mode="w", encoding="utf-8", dir=manifest.parent,
                                     prefix=".ine-manifest-", delete=False) as out:
        json.dump(result, out, ensure_ascii=False, indent=2)
        tmp = out.name
    os.replace(tmp, manifest)
    LOG.info("Manifiesto: %s; obtenidos %s/%s", manifest, len(result["products"]), len(products))
    return int(strict and bool(result["failures"]))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--products", nargs="+", choices=tuple(PAGES), default=list(PAGES))
    parser.add_argument("--manifest", type=Path, default=Path("data/raw/ine_source_manifest.json"))
    parser.add_argument("--strict", action="store_true", help="Error si algún producto falla")
    args = parser.parse_args()
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")
    return run(args.products, args.manifest, args.strict)


if __name__ == "__main__":
    raise SystemExit(main())
