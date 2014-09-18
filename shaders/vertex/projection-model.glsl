#version 130

uniform mat4 projection;
uniform mat4 model;

in vec3 in_Position;
in vec3 in_Normal;

out vec3 norm;

void main(void) {
  gl_Position = projection * model * vec4(in_Position, 1.0);
  norm = in_Position;
}
