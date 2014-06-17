subroutine irpaca(nomcom, ifi, nbordr, iocc, ordr,&
                  nbacc, chacc, nbchca, chamca, nbk16,&
                  nive)
    implicit none
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    character(len=*) :: nomcom, chacc(*), chamca(*)
    integer :: nbordr, ifi, iocc, ordr(*), nbacc, nbchca
    integer :: nbk16, nive
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     IMPRESSION DES VARIABLES D'ACCES SOUS FORME TABLE POUR CASTEM
!     ------------------------------------------------------------------
! IN  NOMCOM   : K8  : NOM DU CONCEPT
! IN  IFI      : I   : NUMERO D'UNITE DU FICHIER D'ECRITURE
! IN  NBORDR   : I   : NOMBRE DE NUMEROS D'ORDRE DU RESULTAT
! IN  IOCC     : I   : NUMERO D'OCCURENCE D'IMPRESSION DU RESULTAT
! IN  ORDR     : I   : LISTE DES NUMEROS D'ORDRE DU RESULTAT
! IN  NBACC    : I   : NOMBRE DE VARIABLES D'ACCES DE LA TABLE CASTEM
! IN  NBCHCA   : I   : NOMBRE DE CHAMPS DE LA TABLE CASTEM
! IN  CHAMCA   : I   : NOMS DES CHAMPS DE LA TABLE CASTEM
! IN  NBK16    : I   : NOMBRE DE VARIABLES D'ACCES DE TYPE K16
! IN  NIVE     : I   : NIVEAU IMPRESSION CASTEM 3 OU 10
!     ---------------------------------------------------------------
!     ------------------------------------------------------------------
    character(len=4) :: ctype, tych
    character(len=8) :: nomco, cfor
    character(len=16) :: toto
    character(len=19) :: noch19
    integer :: maxlen
!-----------------------------------------------------------------------
    integer :: i, iad, ideu, ierd, ior, irest
    integer :: iret, iseize, itype, iun, izero, j
    integer ::  jposi, jtabl, nbobj, nfor, np
!
!-----------------------------------------------------------------------
    parameter ( maxlen = 255)
    character(len=maxlen) :: chaine
    character(len=72) :: ctmp
    integer :: nbcara, iob, il
    integer, pointer :: entier(:) => null()
    integer, pointer :: last(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
    nbobj = nbchca+ nbacc + 1
    call wkvect('&&IRPACA.TABL.CASTEM', 'V V I', nbobj*4, jtabl)
    call wkvect('&&IRPACA.POSI.CASTEM', 'V V I', nbobj, jposi)
    nomco = nomcom
    ideu = 2
    izero = 0
    iun = 1
    chaine = ' '
    nbk16 = 0
!     NBCARA : LONGUEUR CUMULEE DES CLES DE LA TABLE
!     (EX : ORDR, INST, + NOMS CHAMP...)
    iob = 1
    chaine(1:4) = 'ORDR'
    nbcara = 4
    zi(jposi-1+iob) = nbcara
!
! ECRITURE DE ENREGISTREMENT 27
!
    call jeveuo('&&OP0039.LAST', 'E', vi=last)
    do i = 1, nbacc
        toto = chacc(i)
        il = lxlgut(toto)
        if (nbcara+il .gt. maxlen) then
            call utmess('F', 'PREPOST3_5')
        endif
        chaine(nbcara+1:nbcara+il) = toto(1:il)
        nbcara = nbcara + il
        iob = iob + 1
        zi(jposi-1+iob) = nbcara
    end do
    j=0
    do i = 1, nbchca
        call rsexch(' ', nomco, chamca(i), ordr(iocc), noch19,&
                    iret)
        if (iret .eq. 0) then
            j=j+1
            call dismoi('TYPE_CHAMP', noch19, 'CHAMP', repk=tych, arret='C',&
                        ier=ierd)
            if (tych(1:4) .eq. 'NOEU') then
                zi(jtabl-1+(nbacc+1)*4+(j-1)*4+1) = 27
                zi(jtabl-1+(nbacc+1)*4+(j-1)*4+2)=last(8)+i+&
                nbacc+1
                zi(jtabl-1+(nbacc+1)*4+(j-1)*4+3) = 2
                last(4) = last(4) + 1
                zi(jtabl-1+(nbacc+1)*4+(j-1)*4+4) = last(4)
            else if (tych(1:4).eq.'ELNO') then
                zi(jtabl-1+(nbacc+1)*4+(j-1)*4+1) = 27
                zi(jtabl-1+(nbacc+1)*4+(j-1)*4+2)=last(8)+i+&
                nbacc+1
                zi(jtabl-1+(nbacc+1)*4+(j-1)*4+3) = 39
                last(5) = last(5) + 1
                zi(jtabl-1+(nbacc+1)*4+(j-1)*4+4) = last(5)
            endif
        endif
        toto = chamca(i)
        il = lxlgut(toto)
        if (nbcara+il .gt. maxlen) then
            call utmess('F', 'PREPOST3_5')
        endif
        chaine(nbcara+1:nbcara+il) = toto(1:il)
        nbcara = nbcara + il
        iob = iob + 1
        zi(jposi-1+iob) = nbcara
    end do
    itype = 27
    if (iocc .eq. 1) then
        write (ifi,'(A,I4)') ' ENREGISTREMENT DE TYPE',ideu
        if (nive .eq. 3) then
            write (ifi,'(A,I4,A,I4,A,I4)') ' PILE NUMERO',itype,&
            'NBRE OBJETS NOMMES ',izero,'NBRE OBJETS ',nbobj
        else if (nive.eq.10) then
            write (ifi,'(A,I4,A,I8,A,I8)') ' PILE NUMERO',itype,&
            'NBRE OBJETS NOMMES',izero,'NBRE OBJETS',nbobj
        endif
        if (nive .eq. 3) write (ifi,'(2I5)') nbcara,nbobj
        if (nive .eq. 10) write (ifi,'(2I8)') nbcara,nbobj
!        IMPRESSION DES CLES DE LA TABLE PAR TRONCON DE 71 CARACTERES
        irest=nbcara
        nfor=71
        np=nbcara/nfor+1
        call codent(nfor, 'G', ctmp)
        cfor='(1X,A'//ctmp(1:2)//')'
        do i = 1, np
            if (irest .gt. 0) then
                if (irest .gt. nfor) then
                    ctmp(1:nfor)=chaine(nbcara-irest+1:nbcara-irest+&
                    nfor)
                    write(ifi,cfor) ctmp(1:nfor)
                    irest=irest-nfor
                else
                    ctmp(1:irest)=chaine(nbcara-irest+1:nbcara)
                    write(ifi,cfor) ctmp(1:irest)
                    irest=0
                endif
            endif
        end do
!
        if (nive .eq. 3) write(ifi,'(12I5)') (zi(jposi-1+i),i=1,nbobj)
        if (nive .eq. 10) write(ifi,'(10I8)') (zi(jposi-1+i),i=1,nbobj)
!
! ECRITURE DE ENREGISTREMENT ASSOCIE A TOUS LES NUMEROS D'ORDRE
!
        AS_ALLOCATE(vi=entier, size=ideu*nbordr)
        do ior = 1, nbordr
            entier((ior-1)*2+1) = ior
            entier((ior-1)*2+2) = ordr(ior)
        end do
        itype = 26
        write (ifi,'(A,I4)') ' ENREGISTREMENT DE TYPE',ideu
        if (nive .eq. 3) then
            write (ifi,'(A,I4,A,I4,A,I4)') ' PILE NUMERO',itype,&
            'NBRE OBJETS NOMMES ',izero,'NBRE OBJETS ',2*nbordr
            write(ifi,'(I5)') 2*nbordr
        else if (nive.eq.10) then
            write (ifi,'(A,I4,A,I8,A,I8)') ' PILE NUMERO',itype,&
            'NBRE OBJETS NOMMES',izero,'NBRE OBJETS',2*nbordr
            write(ifi,'(I8)') 2*nbordr
        endif
        write(ifi,'(7I11)') (entier(ior),ior=1,2*nbordr)
        last(1) = last(1)+2*nbordr
    endif
    zi(jtabl-1+1) = 27
    zi(jtabl-1+2) = last(8)+iun
    zi(jtabl-1+3) = 26
    zi(jtabl-1+4) = last(7)+iocc*2
!
! ECRITURE DES ENREGISTREMENTS ASSOCIES AUX VARIABLES ACCES
!
    do i = 1, nbacc
        call rsadpa(nomco, 'L', 1, chacc(i), ordr(iocc),&
                    1, sjv=iad, styp=ctype)
        if (ctype(1:1) .eq. 'R') then
            itype = 25
            write (ifi,'(A,I4)') ' ENREGISTREMENT DE TYPE',ideu
            if (nive .eq. 3) then
                write (ifi,'(A,I4,A,I4,A,I4)') ' PILE NUMERO',itype,&
                'NBRE OBJETS NOMMES ',izero,'NBRE OBJETS ',iun
                write(ifi,'(I5)') iun
            else if (nive.eq.10) then
                write (ifi,'(A,I4,A,I8,A,I8)') ' PILE NUMERO',itype,&
                'NBRE OBJETS NOMMES',izero,'NBRE OBJETS',iun
                write(ifi,'(I8)') iun
            endif
            write(ifi,'(1X,1PE21.14)') zr(iad)
            zi(jtabl-1+(i-1)*4+5) = 27
            zi(jtabl-1+(i-1)*4+6) = last(8)+i+1
            zi(jtabl-1+(i-1)*4+7) = 25
            last(2) = last(2) + 1
            zi(jtabl-1+(i-1)*4+8) = last(2)
        else if (ctype(1:1).eq.'I') then
            itype = 26
            write (ifi,'(A,I4)') ' ENREGISTREMENT DE TYPE',ideu
            if (nive .eq. 3) then
                write (ifi,'(A,I4,A,I4,A,I4)') ' PILE NUMERO',itype,&
                'NBRE OBJETS NOMMES ',izero,'NBRE OBJETS ',iun
                write(ifi,'(I5)') iun
            else if (nive.eq.10) then
                write (ifi,'(A,I4,A,I8,A,I8)') ' PILE NUMERO',itype,&
                'NBRE OBJETS NOMMES',izero,'NBRE OBJETS',iun
                write(ifi,'(I8)') iun
            endif
            write(ifi,'(2I11)') zi(iad)
            last(1) = last(1)+1
            zi(jtabl-1+(i-1)*4+5) = 27
            zi(jtabl-1+(i-1)*4+6) = last(8)+i+1
            zi(jtabl-1+(i-1)*4+7) = 26
            zi(jtabl-1+(i-1)*4+8) = last(1)
        else if (ctype(1:3).eq.'K16') then
            itype = 27
            iseize = 16
            write (ifi,'(A,I4)') ' ENREGISTREMENT DE TYPE',ideu
            if (nive .eq. 3) then
                write (ifi,'(A,I4,A,I4,A,I4)') ' PILE NUMERO',itype,&
                'NBRE OBJETS NOMMES ',izero,'NBRE OBJETS ',iun
                write(ifi,'(I5,I5)') iseize,iun
                write(ifi,'(A72)') zk16(iad)
                write(ifi,'(I5)') iseize
            else if (nive.eq.10) then
                write (ifi,'(A,I4,A,I8,A,I8)') ' PILE NUMERO',itype,&
                'NBRE OBJETS NOMMES',izero,'NBRE OBJETS',iun
                write(ifi,'(I8,I8)') iseize,iun
                write(ifi,'(A72)') zk16(iad)
                write(ifi,'(I8)') iseize
            endif
            zi(jtabl-1+(i-1)*4+5) = 27
            zi(jtabl-1+(i-1)*4+6) = last(8)+i+1
            zi(jtabl-1+(i-1)*4+7) = 27
            last(3) = last(3)+1
            zi(jtabl-1+(i-1)*4+8) = last(3)+nbobj
            nbk16 = nbk16 + 1
        endif
    end do
    call jedetr('&&IRPACA.POSI.CASTEM')
    AS_DEALLOCATE(vi=entier)
    call jedema()
end subroutine
