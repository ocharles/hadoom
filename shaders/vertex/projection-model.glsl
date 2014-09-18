#version 130

uniform mat4 projection;
uniform mat4 model;

in vec3 in_Position;
in vec3 in_Normal;

out vec3 norm;
out vec3 worldPos;

void main(void) {
  worldPos = (model * vec4(in_Position, 1)).xyz;
  norm = in_Normal;
  gl_Position = projection * model * vec4(in_Position, 1.0);
}
