---
to: <%= projectName %>/src/Presentation.tsx
---
import React, { Component } from 'react';
import { Deck } from 'spectacle';
import { TitleSlide } from './slides/TitleSlide';
import './Presentation.css';
import createTheme from 'spectacle/lib/themes/default';

const theme = createTheme(
  {
    primary: 'white',
    secondary: '#1F2022',
    tertiary: '#03A9FC',
    quaternary: '#CECECE'
  },
  {
    primary: 'Montserrat',
    secondary: 'Helvetica'
  }
);

export const Presentation = () => (
  <Deck theme={theme}>
    <TitleSlide />
  </Deck>
);
