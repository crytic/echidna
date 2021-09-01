FROM trailofbits/echidna
RUN pip3 install solc-select
COPY entrypoint.sh /entrypoint.sh
RUN chmod ugo+x /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
