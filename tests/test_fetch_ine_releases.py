"""Pruebas del descubrimiento de los Excel del INE; no requieren acceso a internet."""
from __future__ import annotations

import importlib.util
from pathlib import Path
import sys
import unittest

SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "fetch_ine_releases.py"
spec = importlib.util.spec_from_file_location("fetch_ine_releases", SCRIPT)
module = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = module
spec.loader.exec_module(module)


class DiscoveryTests(unittest.TestCase):
    def test_ipi_selects_newest_publication(self):
        page = module.BASE + module.PAGES["ipi"]
        prefix = "/docs/default-source/indice-de-produccion-industrial/cuadros-estadisticos/base-promedio-2018-100/"
        html = "".join(
            f'<a href="{prefix}series-empalmadas-y-mensuales-a-{month}-2026.xls?sfvrsn=abc">'
            f"Series empalmadas y mensuales a {month} 2026</a>"
            for month in ("septiembre", "octubre")
        )
        found = module.extract_candidates(html, page, "ipi")
        self.assertEqual(len(found), 2)
        self.assertEqual(found[0].period, "2026-10")

    def test_reject_old_base_and_foreign_hosts(self):
        page = module.BASE + module.PAGES["ipi"]
        html = """<a href="https://evil.example/docs/default-source/indice-de-produccion-industrial/cuadros-estadisticos/base-promedio-2018-100/series-empalmadas-y-mensuales-a-diciembre-2026.xls">bad</a>
        <a href="/docs/default-source/indice-de-produccion-industrial/cuadros-estadisticos/base-promedio-2014-100/series-empalmadas-y-mensuales-a-diciembre-2026.xls">old</a>"""
        self.assertEqual(module.extract_candidates(html, page, "ipi"), [])

    def test_commerce_and_ivs_do_not_report_2018_as_latest(self):
        cases = {
            "comercio": "/docs/default-source/actividad-mensual-del-comercio/cuadros-estadisticos/base-promedio-a%C3%B1o-2018-100/series-mensuales-desde-enero-de-2018-a-la-fecha.xls",
            "ivs": "/docs/default-source/ventas-de-servicios/cuadro-estadisticos/base-promedio-a%C3%B1o-2018-100/series_mensuales_desde_enero_2018_a_la_fecha.xls",
        }
        for product, uri in cases.items():
            html = f'<a href="{uri}">Series mensuales desde enero de 2018 a la fecha</a>'
            found = module.extract_candidates(html, module.BASE + module.PAGES[product], product)
            self.assertEqual(len(found), 1)
            self.assertIsNone(found[0].period)

    def test_reject_non_excel_response(self):
        self.assertEqual(module.file_kind(bytes.fromhex("d0cf11e0a1b11ae1") + b"\0"*5000), ".xls")
        self.assertEqual(module.file_kind(b"PK\x03\x04" + b"\0"*5000), ".xlsx")
        with self.assertRaises(ValueError):
            module.file_kind(b"<!doctype html>Access denied")

    def test_serialized_url_without_link_text(self):
        html = """<script>var u="/docs/default-source/indice-de-produccion-industrial/cuadros-estadisticos/base-promedio-2018-100/series-empalmadas-y-mensuales-a-septiembre-2026.xls?sfvrsn=d32cfee4_6";</script>"""
        found = module.extract_candidates(html, module.BASE + module.PAGES["ipi"], "ipi")
        self.assertEqual(found[0].period, "2026-09")


if __name__ == "__main__":
    unittest.main()
