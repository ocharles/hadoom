#version 130

out vec4 fragColor;

in vec3 norm;

void main(void) {
  fragColor = vec4((norm + 25) / 50, 1.0);
}
