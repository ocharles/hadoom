#version 130

uniform mat4 projection;
uniform mat4 view;

in vec3 in_Position;
in vec3 in_Normal;
in vec2 in_UV;

out vec3 norm;
out vec3 worldPos;

void main(void) {
  vec4 wp = view * vec4(in_Position, 1);
  norm = in_Normal;
  worldPos = wp.xyz;
  gl_Position = projection * wp;
  gl_TexCoord[0] = vec4(in_UV, 0, 0);
}
