#version 330 core

// Ouput data
layout(location = 0) out float fragmentdepth;

void main(){
	fragmentdepth = exp(80 * gl_FragCoord.z);
}
