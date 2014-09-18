#version 130

out vec4 fragColor;

in vec3 norm;
in vec3 worldPos;

uniform vec3 lightPos;

uniform sampler2D tex;

void main(void) {
  vec3 dir = lightPos - worldPos;
  fragColor = (vec4(0.2) + texture2D(tex, gl_TexCoord[0].xy)) * clamp(dot(norm, dir), 0, 1) * 750 / pow(length(dir), 2);
}
