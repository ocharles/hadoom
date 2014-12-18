#version 400 core

// Ouput data
layout(location = 0) out highp uvec4 fragmentdepth;

in vec3 wp;

void main(){
  float near = 1.0f;
  float far = 100.0f;
  float z = (length(wp - vec3(0, 2, 0)) - near) / (far - near);

  float scale = 1073741824; // 2 ^ 30

  fragmentdepth = uvec4(round(vec4(z * scale)));
}
