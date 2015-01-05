open Utils

type tileset = {
  image : Sdlvideo.surface;
  tile_size : int;
  rows : int;
  cols : int;
}

type layer =
  | Uniform of Sdlvideo.color
  | Tiled of tileset * int Grid.t

type t = {
  layers : layer list;
}

let make_tileset ~image_file ~tile_size =
  try
    let image = Sdlloader.load_image image_file in
    let w, h, _ = Sdlvideo.surface_dims image
    in {
      image = image;
      tile_size = tile_size;
      rows = h / tile_size;
      cols = w / tile_size;
    }
  with Sdlloader.SDLloader_exception e -> failwith ("make_tileset: " ^ e)