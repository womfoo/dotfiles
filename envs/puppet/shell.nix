with import <nixpkgs> {}; {
  rubyEnv = stdenv.mkDerivation {
    name = "puppet-env";
    buildInputs = [
      ruby_2_4
      facter
      augeas
    ];
    shellHook = ''
      export SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt
      export RUBYLIB=${facter}/lib/ruby
      export GEM_PATH=/home/kranium/.gem/ruby/2.4.0
      export PATH=$PATH:~/.gem/ruby/2.4.0/bin
    '';
  };
}

# gem install puppet --user
# gem install ruby-augeas --user
