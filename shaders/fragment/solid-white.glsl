#version 130

out vec4 fragColor;

in vec3 norm;
in vec3 worldPos;

void main(void) {
  vec3 dir = vec3(0, 0, 0) - worldPos;
  fragColor = vec4(vec3(1, 0.5, 0) * clamp(dot(norm, dir), 0, 1) * 750 / pow(length(dir), 2), 1.0);
}
