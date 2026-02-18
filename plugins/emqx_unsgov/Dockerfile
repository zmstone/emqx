FROM emqx/emqx:6.1.1-rc.4

COPY _build/default/emqx_plugrel/emqx_unsgov-0.1.0.tar.gz /opt/emqx/plugins/

RUN set -eux; \
    cd /opt/emqx/plugins; \
    tar -xzf emqx_unsgov-0.1.0.tar.gz

RUN cat >> /opt/emqx/etc/base.hocon <<'EOF'
plugins {
  install_dir = plugins
  states = [
    {
      enable = true
      name_vsn = "emqx_unsgov-0.1.0"
    }
  ]
}
EOF

RUN echo "admin:public" > /opt/emqx/data/emqx-bootstrap-api-keys
ENV EMQX_API_KEY__BOOTSTRAP_FILE=/opt/emqx/data/emqx-bootstrap-api-keys
