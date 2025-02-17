type tile = {
  x: int;
  y: int;
  texture_id: int;
}

type map = {
  width: int;
  height: int;
  tiles: tile list;
}