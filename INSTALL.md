# Passos para execução do projeto

O roteiro abaixo foi testado em sistemas Linux. Funcionou com sucesso no Ubuntu 22.4 e Fedora 38.

## Haskell
Para execução é necessária instalação do GCH, Cabal, entre outros.
Recomemdamos a instalação via GHCup (https://www.haskell.org/ghcup/), que possui as versões mais recentes dos programas.
Durante a execução o ghcup vai apresentar as bibliotecas necessárias. Certifique-se se instalá-las.<br>
<code> curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh </code>


## Demais programas necessários

Os programas `alex`, `bnfc`* e `happy` são necesários à execução do projeto.<br> Instale-os com o cabal:<br>
<code>cabal install alex</code>
<code>cabal install happy</code>
<code>cabal install BNFC</code>

\* Possivelmente o bnfc já estará instalado numa versão anterior. Instale-o novamente para buscar a versão mais recente. O projeto foi executado na versão 2.9.4.1

## CUDD
O projeto necessita, ainda, do CUDD para ser executado. Siga as instruções abaixo:

Faça um clone do projeto e acesse a pasta:<br>
<code>git clone https://github.com/ivmai/cudd</code><br>
<code>cd cudd</code><br>

Configure, teste e instale o cudd:<br>
<code>./configure --enable-obj --enable-dddmp --enable-shared</code><br>
<code>make</code><br>
<code>make check</code><br>
<code>sudo make install</code><br>

Ainda na pasta cudd, copie alguns arquivos necessários foram copiados no processo:<br>
<code>sudo cp config.h /usr/local/include/</code><br>
<code>sudo cp util/util.h /usr/local/include/</code><br>
<code>sudo cp mtr/mtr.h /usr/local/include/</code><br>

Por algum motivo, o cabal não está reconhecendo os argumentos "--extra-include-dirs=" e "--extra-lib-dirs=", então é necessário passar informações adicionais para o cabal na instalação do CUDD e no primeiro build do projeto. Adicionalmente, precisamos passar o argumento --lib, para que o cabal disponibilize o cudd como uma biblioteca e não como executável:<br>
<code>C_INCLUDE_PATH=/usr/local/include LIBRARY_PATH=/usr/local/lib/ cabal install --lib cudd</code>

Acesse novamente esse projeto, e execute o build:<br>
<code>C_INCLUDE_PATH=/usr/local/include LIBRARY_PATH=/usr/local/lib/ cabal build</code>

A partir de agora, pode executar o projeto normalmente*:<br>
<code>cabal run < src/Language/Examples/Concat.lng</code> 

Verifique no arquivo app/Main.js qual programa será executado (Typecheck, Base, Var, etc.) e qual input está sendo enviado no Driver.hs correspondente.