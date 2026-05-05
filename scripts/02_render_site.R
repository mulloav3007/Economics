# ============================================================
# 02_render_site.R
# Renderiza el sitio Quarto completo
# ============================================================

if (!requireNamespace("quarto", quietly = TRUE)) {
  stop("Falta el paquete quarto. Instala con: install.packages('quarto')")
}

quarto::quarto_render()
