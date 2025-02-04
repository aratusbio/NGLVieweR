devtools::load_all(".")
library(dplyr)
# library(Rpdb) # requires rgl and x11
# http://thegrantlab.org/bio3d/articles/online/pdb_vignette/Bio3D_pdb.html
library(bio3d)

# You can download the pdb files for this analysis with this link:
# 
# http://60.205.184.249:8000/csservice/item/Q8TBR7/structure/alphafold/AF-Q8TBR7-F1-model_v1/cavity_result/AF-Q8TBR7-F1-model_v1_cavity_result_1.tar.gz
pdb.dir <- system.file("extdata/cavityspace/example", package = "chemoproteomicsi")

# TLCD3A
uniprot_id <- "Q8TBR7"

# This won't work w/o internet
# apdb <- fetch_alphafold_pdb(uniprot_id, version = "all")
# v1 <- filter(apdb, version == 1)
# stopifnot(nrow(v1) == 1L)

fn <- list(
  # structure = v1$url,
  structure = file.path(pdb.dir, sprintf("AF-%s-F1-model_v1.pdb", uniprot_id)),
  cavity = file.path(pdb.dir, sprintf("AF-%s-F1-model_v1_cavity_1.pdb", uniprot_id)),
  # vacant = file.path(pdb.dir, sprintf("AF-%s-F1-model_v1_vacant_1.pdb", uniprot_id)),
  surface = file.path(pdb.dir, sprintf("AF-%s-F1-model_v1_surface_1.pdb", uniprot_id)))
stopifnot(all(file.exists(unlist(fn))))

pdb <- lapply(fn, bio3d::read.pdb)

## note struct_protein == pdb$structure
# for res in struct_protein[0]['A']:
#   if res.resname == 'CYS':
#   for atom in res:
#   atom.set_bfactor(3.0)   # set bfactor as 3 for liganded Cys145.
# I think we are changing the valule of the `b` column to 3 fo all tye cysteines
# TODO: verify
pdb$structure$atom_original <- pdb$structure$atom
pdb$structure$atom <- mutate(
  pdb$structure$atom_original,
  b = ifelse(resid == "CYS", 3.0, b))

# with open(surface_pdb) as file:
#   position = [line.rstrip().split()[5:8] for line in file if line.startswith('HETATM')]
# A 4 column data.frame: x,y,z
position <- pdb$surface$atom |> 
  filter(type == "HETATM") |> 
  select(x, y, z)
head(position, 3)
pos.mat <- as.matrix(position)
#   x    y   z
# -19 -6.0 6.0
# -19 -6.0 6.5
# -19 -5.5 5.5


sphere_buffer <- list(
  position = position |> as.matrix() |> t() |> as.character(),
  color = rep(c(1, 0, 0), nrow(position)),
  radius = rep(0.1, nrow(position)))
sapply(sphere_buffer, length)
# position    color   radius 
#    10329    10329     3443 
# selec_cavity = ' or '.join([str(res.id[1]) for res in struct_cavity[0]['A']])
selec_cavity <- pdb$cavity$atom |> 
  filter(chain == "A") |> 
  distinct(resno, .keep_all = TRUE) |> 
  pull(resno) |> 
  paste(collapse = " or ")


nv <- NGLVieweR(fn$structure) |> 
  stageParameters(backgroundColor = "white", zoomSpeed = 1) |> 
  addRepresentation(
    type = "cartoon",
    param = list(name = "cartoon", colorScheme = "residueindex")) |> 
  # addRepresentation(
  #   type = "ball+stick",
  #   param = list(
  #     # colorValue = "red",
  #     # colorScheme = "element",
  #     sele = "sidechainAttached AND Cys")) |> 
  addRepresentation("ball+stick", param = list(
    sele = sprintf("sidechainAttached AND (%s)", selec_cavity),
    colorValue = "red")) |> 
  addSphere(
    position = pos.mat,
    color = c(1, 0, 0),
    radius = 0.25)

# Add a selection over disjoin regions to simulate highlighting AA positions
# across the protein. We'll highlight positions
aapos <- c(13 ,204, 226)
hstart <- aapos - 2
hend <- aapos + 2
aa.select <- paste(sprintf("%d-%d", hstart, hend), collapse = " or ")

nv2 <- nv |> 
  addRepresentation(
    "cartoon",
    param = list(
      name = "region",
      sele = aa.select,
      opacity = 1,
      # colorValue = "#517FE8",
      colorValue = "#B77EB8",
      # colorValue = "#00FF00",
      colorScheme = "uniform"))
