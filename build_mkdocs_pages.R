files <- list.files(
  "docs/plots/countries/sectors",
  pattern = "\\.svg$",
  full.names = TRUE
)

countries <- sub(".*/(.*)-sectors\\.svg$", "\\1", files)

country_ids <- tolower(gsub("[^a-zA-Z0-9]+", "-", countries))

# -------------------------
# Header
# -------------------------
out <- c("---
title: Sector figures
hide:
  - navigation
  - toc
---\n")


# -------------------------
# Text
# -------------------------
out <- c(out,"These figures show emissions aggregated across all greenhouse gases to 9 sectors, taking the main categories of national GHG inventories, but also breaking down the Energy sector into its 5 main subsectors.")

out <- c(out,"\nPlease cite the figures or data as follows: Lamb, W. F. (2026). Tidy GHG Inventories (v2) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.14512139")


# -------------------------
# Dropdown menu
# -------------------------
countries <- sub(".*/(.*)-sectors\\.svg$", "\\1", files)
country_ids <- tolower(gsub("[^a-zA-Z0-9]+", "-", countries))

index_dropdown <- paste0(
  "<div class='figure-index-dropdown'>",
  "<select onchange=\"if (this.value) window.location.hash=this.value\">",
  "<option value=''>Jump to country…</option>",
  paste0(
    "<option value='", country_ids, "'>",
    countries,
    "</option>",
    collapse = "\n"
  ),
  "</select>",
  "</div>\n\n"
)

out <- c(out,
  index_dropdown
)

# -------------------------
# Figures
# -------------------------
out <- c(out,"<div class='grid cards' markdown>\n")
for (file in files) {
  
  rel <- gsub("^docs/", "", file)
  country <- sub(".*/(.*)-sectors\\.svg$", "\\1", file)
  country_id <- tolower(gsub("[^a-zA-Z0-9]+", "-", country))
  
  link_base <- "https://raw.githubusercontent.com/lambwf/Tidy-GHG-Inventories/main/plots/countries/sectors/"
  
  link_data <- paste0("<a target='_blank' href='", link_base, country, "-sectors.xlsx'>data</a>")
  link_png  <- paste0("<a target='_blank' href='", link_base, country, "-sectors.png'>png</a>")
  link_pdf  <- paste0("<a target='_blank' href='", link_base, country, "-sectors.pdf'>pdf</a>")
  link_svg  <- paste0("<a target='_blank' href='", link_base, country, "-sectors.svg'>svg</a>")
  
  card <- paste(
    paste0("- <span id='", country_id, "'></span>"),
    paste0("  ![](", rel, ")"),
    paste0(
      "  <span class='figure-links'>",
      link_data, " | ",
      link_png, " | ",
      link_pdf, " | ",
      link_svg,
      "</span>"
    ),
    "",
    sep = "\n"
  )
  
  out <- c(out, card)
}

out <- c(out, "\n</div>")

writeLines(out, "docs/sectors.md")


########### Fuels page ########### 
rm(out)

files <- list.files(
  "docs/plots/countries/sankeys",
  pattern = "\\.svg$",
  full.names = TRUE
)

countries <- sub(".*/(.*)-sankey\\.svg$", "\\1", files)

country_ids <- tolower(gsub("[^a-zA-Z0-9]+", "-", countries))

# -------------------------
# Header
# -------------------------
out <- c("---
title: Sector figures
hide:
  - navigation
  - toc
---\n")


# -------------------------
# Text
# -------------------------
out <- c(out,"These figures show emissions mapped across fuels, sectors and greenhouse gases. In most cases the contribution of fossil fuels to industrial processes is unknown.")

out <- c(out,"\nPlease cite the figures or data as follows: Lamb, W. F. (2026). Tidy GHG Inventories (v2) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.14512139")


# -------------------------
# Dropdown menu
# -------------------------
countries <- sub(".*/(.*)-sankey\\.svg$", "\\1", files)
country_ids <- tolower(gsub("[^a-zA-Z0-9]+", "-", countries))

index_dropdown <- paste0(
  "<div class='figure-index-dropdown'>",
  "<select onchange=\"if (this.value) window.location.hash=this.value\">",
  "<option value=''>Jump to country…</option>",
  paste0(
    "<option value='", country_ids, "'>",
    countries,
    "</option>",
    collapse = "\n"
  ),
  "</select>",
  "</div>\n\n"
)

out <- c(out,
         index_dropdown
)

# -------------------------
# Figures
# -------------------------
out <- c(out,"<div class='grid cards' markdown>\n")
for (file in files) {
  
  rel <- gsub("^docs/", "", file)
  country <- sub(".*/(.*)-sankey\\.svg$", "\\1", file)
  country_id <- tolower(gsub("[^a-zA-Z0-9]+", "-", country))
  
  link_base <- "https://raw.githubusercontent.com/lambwf/Tidy-GHG-Inventories/main/plots/countries/sankeys/"
  
  link_data <- paste0("<a target='_blank' href='", link_base, country, "-sankey.xlsx'>data</a>")
  link_png  <- paste0("<a target='_blank' href='", link_base, country, "-sankey.png'>png</a>")
  link_pdf  <- paste0("<a target='_blank' href='", link_base, country, "-sankey.pdf'>pdf</a>")
  link_svg  <- paste0("<a target='_blank' href='", link_base, country, "-sankey.svg'>svg</a>")
  
  card <- paste(
    paste0("- <span id='", country_id, "'></span>"),
    paste0("  ![](", rel, ")"),
    paste0(
      "  <span class='figure-links'>",
      link_data, " | ",
      link_png, " | ",
      link_pdf, " | ",
      link_svg,
      "</span>"
    ),
    "",
    sep = "\n"
  )
  
  out <- c(out, card)
}

out <- c(out, "\n</div>")

writeLines(out, "docs/fuels.md")


########### Versions page ########### 
rm(out)

files <- list.files(
  "docs/plots/countries/versions",
  pattern = "\\.svg$",
  full.names = TRUE
)

countries <- sub(".*/(.*)-versions\\.svg$", "\\1", files)

country_ids <- tolower(gsub("[^a-zA-Z0-9]+", "-", countries))

# -------------------------
# Header
# -------------------------
out <- c("---
title: Sector figures
hide:
  - navigation
  - toc
---\n")


# -------------------------
# Text
# -------------------------
out <- c(out,"These figures show aggregate emissions across different national GHG inventory submissions. More non-Annex I countries will become available once they submit new Biennial Transparency Reports.")

out <- c(out,"\nPlease cite the figures or data as follows: Lamb, W. F. (2026). Tidy GHG Inventories (v2) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.14512139")

# -------------------------
# Dropdown menu
# -------------------------
countries <- sub(".*/(.*)-versions\\.svg$", "\\1", files)
country_ids <- tolower(gsub("[^a-zA-Z0-9]+", "-", countries))

index_dropdown <- paste0(
  "<div class='figure-index-dropdown'>",
  "<select onchange=\"if (this.value) window.location.hash=this.value\">",
  "<option value=''>Jump to country…</option>",
  paste0(
    "<option value='", country_ids, "'>",
    countries,
    "</option>",
    collapse = "\n"
  ),
  "</select>",
  "</div>\n\n"
)

out <- c(out,
         index_dropdown
)

# -------------------------
# Figures
# -------------------------
out <- c(out,"<div class='grid cards' markdown>\n")
for (file in files) {
  
  rel <- gsub("^docs/", "", file)
  country <- sub(".*/(.*)-versions\\.svg$", "\\1", file)
  country_id <- tolower(gsub("[^a-zA-Z0-9]+", "-", country))
  
  link_base <- "https://raw.githubusercontent.com/lambwf/Tidy-GHG-Inventories/main/plots/countries/versions/"
  
  link_data <- paste0("<a target='_blank' href='", link_base, country, "-versions.xlsx'>data</a>")
  link_png  <- paste0("<a target='_blank' href='", link_base, country, "-versions.png'>png</a>")
  link_pdf  <- paste0("<a target='_blank' href='", link_base, country, "-versions.pdf'>pdf</a>")
  link_svg  <- paste0("<a target='_blank' href='", link_base, country, "-versions.svg'>svg</a>")
  
  card <- paste(
    paste0("- <span id='", country_id, "'></span>"),
    paste0("  ![](", rel, ")"),
    paste0(
      "  <span class='figure-links'>",
      link_data, " | ",
      link_png, " | ",
      link_pdf, " | ",
      link_svg,
      "</span>"
    ),
    "",
    sep = "\n"
  )
  
  out <- c(out, card)
}

out <- c(out, "\n</div>")

writeLines(out, "docs/versions.md")






