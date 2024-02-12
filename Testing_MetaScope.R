# if (!requireNamespace("devtools", quietly = TRUE))
#   install.packages("devtools")
# devtools::install_github("wejlab/MetaScope")

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
# BiocManager::install("BiocStyle")
BiocManager::install("MetaScope")
# install.packages(c("boot", "nlme", "curl", "bit", "gargle", "highr", "isoband", "openssl", "ragg", "textshaping"))
suppressPackageStartupMessages({
  library(MetaScope)
  library(magrittr)
})
NCBI_key <- "01d22876be34df5c28f4aedc479a2674c809"
options("ENTREZ_KEY" = NCBI_key)

target_ref_temp <- tempfile()
dir.create(target_ref_temp)

all_species <- c("Staphylococcus aureus RF122",
                 "Staphylococcus aureus subsp. aureus ST398",
                 "Staphylococcus aureus subsp. aureus Mu3")
sapply(all_species, download_refseq,
       reference = FALSE, representative = FALSE, compress = TRUE,
       out_dir = target_ref_temp, caching = TRUE)

filter_ref_temp <- tempfile()
dir.create(filter_ref_temp)

download_refseq(
  taxon = "Staphylococcus epidermidis RP62A",
  representative = FALSE, reference = FALSE,
  compress = TRUE, out_dir = filter_ref_temp,
  caching = TRUE)

# Get barcode, index, and read data locations
barcodePath <-
  system.file("extdata", "barcodes.txt", package = "MetaScope")
indexPath <- system.file("extdata", "virus_example_index.fastq",
                         package = "MetaScope")
readPath <-
  system.file("extdata", "virus_example.fastq", package = "MetaScope")

# Get barcode, index, and read data locations
demult <-
  demultiplex(barcodePath,
              indexPath,
              readPath,
              rcBarcodes = FALSE,
              hammingDist = 2,
              location = tempfile())

# Create temp directory to store the Bowtie2 indices
index_temp <- tempfile()
dir.create(index_temp)

# Create target index
mk_bowtie_index(
  ref_dir = target_ref_temp,
  lib_dir = index_temp,
  lib_name = "target",
  overwrite = TRUE
)

# Create filter index
mk_bowtie_index(
  ref_dir = filter_ref_temp,
  lib_dir = index_temp,
  lib_name = "filter",
  overwrite = TRUE
)

# Create a temp directory to store output bam file
output_temp <- tempfile()
dir.create(output_temp)

# Get path to example reads
readPath <-
  system.file("extdata", "reads.fastq", package = "MetaScope")

# Align reads to the target genomes
target_map <-
  align_target_bowtie(
    read1 = readPath,
    lib_dir = index_temp,
    libs = "target",
    align_dir = output_temp,
    align_file = "bowtie_target",
    overwrite = TRUE
  )

final_map <-
  filter_host_bowtie(
    reads_bam = target_map,
    lib_dir = index_temp,
    libs = "filter",
    make_bam = TRUE, # Set to true to create BAM output
    # Default is to create simplified .csv.gz output
    # The .csv.gz output is much quicker to create!
    overwrite = TRUE,
    threads = 1
  )

bamFile <- Rsamtools::BamFile(final_map)

param <-
  Rsamtools::ScanBamParam(
    flag = Rsamtools::scanBamFlag(isSecondaryAlignment = FALSE),
    what = c("flag", "rname")
  ) #Gets info about primary alignments

aln <- Rsamtools::scanBam(bamFile, param = param)
accession_all <- aln[[1]]$rname
unique_accession_all <- unique(accession_all)
accession_all_inds <- match(accession_all, unique_accession_all)
unique_accession_genome_name <- suppressMessages(
  taxize::genbank2uid(unique_accession_all,
                      batch_size = length(unique_accession_all))) %>%
  vapply(function(x) attr(x, "name"), character(1))

genome_name_all <- unique_accession_genome_name[accession_all_inds] %>%
  gsub(',.*', '', .) %>%
  gsub("(ST398).*", "\\1", .) %>%
  gsub("(N315).*", "\\1", .) %>%
  gsub("(Newman).*", "\\1", .) %>%
  gsub("(Mu3).*", "\\1", .) %>%
  gsub("(Mu50).*", "\\1", .) %>%
  gsub("(RF122).*", "\\1", .)
read_count_table <- sort(table(genome_name_all), decreasing = TRUE)
knitr::kable(
  read_count_table,
  col.names = c("Genome Assigned", "Read Count"))

bamFile <- Rsamtools::BamFile(final_map)

param <-
  Rsamtools::ScanBamParam(
    flag = Rsamtools::scanBamFlag(isSecondaryAlignment = TRUE),
    what = c("flag", "rname")
  ) #Gets info about secondary alignments

aln <- Rsamtools::scanBam(bamFile, param = param)
accession_all <- aln[[1]]$rname
unique_accession_all <- unique(accession_all)
accession_all_inds <- match(accession_all, unique_accession_all)
unique_accession_taxid <-
  suppressMessages(
    taxize::genbank2uid(unique_accession_all,
                        batch_size = length(unique_accession_all)))
unique_accession_genome_name <-
  vapply(unique_accession_taxid, function(x)
    attr(x, "name"), character(1))
genome_name_all <- unique_accession_genome_name[accession_all_inds]
genome_name_all <- gsub(',.*', '', genome_name_all)
genome_name_all <- gsub("(ST398).*", "\\1", genome_name_all)
genome_name_all <- gsub("(N315).*", "\\1", genome_name_all)
genome_name_all <- gsub("(Newman).*", "\\1", genome_name_all)
genome_name_all <- gsub("(Mu3).*", "\\1", genome_name_all)
genome_name_all <- gsub("(Mu50).*", "\\1", genome_name_all)
genome_name_all <- gsub("(RF122).*", "\\1", genome_name_all)
read_count_table <- sort(table(genome_name_all), decreasing = TRUE)
knitr::kable(
  read_count_table,
  col.names = c("Genome Assigned", "Read Count"))

metascope_id(
  final_map,
  input_type = "bam",
  # change input_type to "csv.gz" when not creating a BAM
  aligner = "bowtie2",
  num_species_plot = 0
)

relevant_col <- dirname(final_map) %>%
  file.path("bowtie_target.metascope_id.csv") %>%
  read.csv() %>% dplyr::select(2:4)

relevant_col |>
  dplyr::mutate(
    Genome = stringr::str_replace_all(Genome, ',.*', ''),
    Genome = stringr::str_replace_all(Genome, "(ST398).*", "\\1"),
    Genome = stringr::str_replace_all(Genome, "(N315).*", "\\1"),
    Genome = stringr::str_replace_all(Genome, "(Newman).*", "\\1"),
    Genome = stringr::str_replace_all(Genome, "(Mu3).*", "\\1"),
    Genome = stringr::str_replace_all(Genome, "(RF122).*", "\\1")
  ) |>
  knitr::kable()

unlink(".bowtie2.cerr.txt")