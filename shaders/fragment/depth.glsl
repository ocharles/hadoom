#version 400 core

// Ouput data
layout(location = 0) out highp vec4 fragmentdepth;

in vec3 wp;

const float c = 10.0f;

void main(){
  float near = 1.0f;
  float far = 100.0f;
  float z = (length(wp - vec3(0, 2, 0)) - near) / (far - near);

  float scale = 1073741824; // 2 ^ 30

  fragmentdepth = vec4(exp(c * z), exp(c * z) * z, 0, 0);
}
