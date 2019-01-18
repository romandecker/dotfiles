---
to: <%= projectName %>/src/slides/TitleSlide.tsx
---
import React from 'react';
import { Slide, Heading, Image } from 'spectacle';
import logo from './logo.png';

export const TitleSlide = () => (
  <Slide>
    <Heading>Title</Heading>
    <Image src={logo} />
  </Slide>
);
