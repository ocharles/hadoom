#version 330 core

layout(location = 0) in vec3 in_Position;

uniform mat4 depthV;
uniform mat4 depthP;

out vec3 wp;

void main(){
	gl_Position = depthP * depthV * vec4(in_Position,1);
    wp = in_Position;
}
