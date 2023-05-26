const nextMDX = require('@next/mdx');

nextMDX
/** @type {import('next').NextConfig} */
const nextConfig = {
    pageExtensions: ['tsx', 'ts', 'mdx'],
}

const withMDX = nextMDX({
    options: {
      remarkPlugins: [],
      rehypePlugins: [],
    },
  });
  

module.exports = withMDX( nextConfig)
