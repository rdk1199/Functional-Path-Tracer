A (supposedly) physically accurate path tracer written entirely in OCaml, using functional programming only (with some exceptions for I/O). 
Just meant to be my own quick introduction to FP/OCaml - not a super fully featured renderer.
No command line I/O, so the image size, number of samples, files, etc. have to be specified manually in Main.ml
Supports obj/mtl files with no face coordinates or normals.

Reflections use the Cook-Torrance reflectance model with the GGX normal distribution function. Perfect refraction only (i.e. no nontrivial BTDF)

Cornell box showing off some effects (not many samples though):
<img width="600" height="600" alt="mt_cornell_box_ggx_refract_glass_tail_rec_check" src="https://github.com/user-attachments/assets/bebe5312-424c-4b9a-8ebf-f2e525b3dd73" />

