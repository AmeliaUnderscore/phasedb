# syntax=docker/dockerfile:1
#
# phasedb-ui, React + Vite admin UI for PhaseDB (TypeScript).
#
# Multi-stage:
#   builder : node:22 builds the app (`tsc` + `vite build` -> dist/)
#   runner  : nginx serves dist/ on port 80 and proxies /v1/ -> PhaseDB.
# The app's API client defaults BASE to the same origin (""), so /v1/* is
# handled by nginx inside the container and never leaves it.
#
#   docker build -t phasedb-ui:latest -f phasedb-ui/Dockerfile .
#   docker run --rm -p 5173:80 phasedb-ui:latest
#
# Dev (hot reload) on the host instead, point the app at PhaseDB:
#   export VITE_API_BASE=http://127.0.0.1:8484
#   cd phasedb-ui && npm run dev   # -> http://localhost:5173

FROM node:22-bookworm AS builder

WORKDIR /build

COPY phasedb-ui/package.json phasedb-ui/package-lock.json ./
RUN npm ci

COPY phasedb-ui/ ./

RUN npm run build

FROM nginx:1.27-alpine AS phasedb-ui

# Serve the production build.
COPY --from=builder /build/dist /usr/share/nginx/html

# SPA fallback (React Router) + /v1 proxy to the PhaseDB service. nginx is
# the single origin so the browser never sees a cross-origin /v1 fetch.
RUN printf '%s\n' \
    'server {' \
    '  listen 80;' \
    '  server_name _;' \
    '  root /usr/share/nginx/html;' \
    '  index index.html;' \
    '  location /v1/ {' \
    '    proxy_pass http://phasedb-hs:8484;' \
    '    proxy_http_version 1.1;' \
    '    proxy_set_header Host $host;' \
    '    proxy_set_header X-Real-IP $remote_addr;' \
    '    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;' \
    '    proxy_set_header X-Forwarded-Proto $scheme;' \
    '  }' \
    '  location / {' \
    '    try_files $uri $uri/ /index.html;' \
    '  }' \
    '  access_log off;' \
    '  error_log off;' \
    '}' > /etc/nginx/conf.d/default.conf

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
