subroutine coqucf(nomu)
    implicit none
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/cescar.h"
#include "asterfort/cesexi.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/fointe.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
    character(len=8) :: nomu
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!                          AFFE_CARA_ELEM
!
!     POUR LES COQUES ET LES GRILLES IL PEUT EXISTER UNE CARTE DE
!     FONCTIONS DE (X,Y,Z) DONNANT :
!        EPAISSEUR
!        SECTION
!        EXCENTREMENT
!
!     IL FAUT EVALUER LES FONCTIONS AU CENTRE DE GRAVITE DE L'ELEMENT
!     ET REMPLIR LA CARTE DE REELS
! ----------------------------------------------------------------------
!
!
    integer :: ifm, niv, iret, ibid, ii, jj, kk
    integer :: jcesdf, jcesdo, jcescf, jcesco, nbmail, adrm, iad
    integer :: nbcmpf, nbcmpo, icompo, inoeu, nbno, nunoe, igeom
    integer :: jceslf, jceslo, jcesvf, jcesvo
    integer :: jconne, iadr1, iadr2, jtabco
    real(kind=8) :: valr(3), fresu
    character(len=8) :: nomma, nmcmpf, nomval(3), nomfct
    character(len=19) :: cartco, cartcf, celsco, celscf, connex
    character(len=24) :: k24bid
    logical :: lcoor
!
    data   nomval  /'X','Y','Z'/
! ----------------------------------------------------------------------
    call jemarq()
!
! --- CARTE POUR LES FONCTIONS
    cartcf = nomu//'.CARCOQUF'
    call exisd('CARTE', cartcf, iret)
!     SI LA CARTE DE FONCTIONS N'EXISTE PAS : RIEN A FAIRE
    if (iret .eq. 0) goto 9999
!
! --- CARTE POUR LES VALEURS REELLES
    cartco = nomu//'.CARCOQUE'
    call exisd('CARTE', cartco, iret)
!     SI LA CARTE DE REELS N'EXISTE PAS : BIZARRE
    ASSERT(iret.ne.0)
!     POUR TRACE DES INFORMATIONS DANS LE FICHIER DE MESSAGES
    call infniv(ifm, niv)
!
    celsco = '&&COQUCF.CHSCO'
    celscf = '&&COQUCF.CHSCF'
! --- TRANSFORMATION DE LA CARTE DE REELS EN ELEM_S
    call carces(cartco, 'ELEM', ' ', 'V', celsco,&
                'A', ibid)
! --- TRANSFORMATION DE LA CARTE DE FONCTIONS EN ELEM_S
    call carces(cartcf, 'ELEM', ' ', 'V', celscf,&
                'A', ibid)
    if (niv .ge. 2) then
        write(ifm,'(A)') ' '
        write(ifm,'(A)') 'TRANSFORMATION DES CARTES EN CHELEM_S'
        write(ifm,'(A)') '  <'//cartco//'>  <'//cartcf//'>'
    endif
!
! --- VERIFICATION QUE LES NOMS DANS LES DEUX ELEM_S EXISTENT
!        RECUPERATION DES NOMS DES COMPOSANTES DANS CELSCF
!        VERIFICATION DE L'EXISTANCE DANS CELSCO
    call jeveuo(celscf//'.CESD', 'L', jcesdf)
    call jeveuo(celsco//'.CESD', 'L', jcesdo)
    nbcmpf = zi(jcesdf+1)
    nbcmpo = zi(jcesdo+1)
!     ADRESSE DES NOMS DES COMPOSANTES
    call jeveuo(celscf//'.CESC', 'L', jcescf)
    call jeveuo(celsco//'.CESC', 'L', jcesco)
    85 format('(',i3,'A9)')
    if (niv .ge. 2) then
        write(ifm,'(A,I3)') 'COMPOSANTES DE <'//cartco//'> ',nbcmpo
        write(k24bid,85) nbcmpo
        write(ifm,k24bid) (zk8(jcesco+jj),jj=0, nbcmpo-1)
        write(ifm,'(A,I3)') 'COMPOSANTES DE <'//cartcf//'> ',nbcmpf
        write(k24bid,85) nbcmpf
        write(ifm,k24bid) (zk8(jcescf+jj),jj=0, nbcmpf-1)
    endif
!
    iret = 0
    do 100 ii = 1, nbcmpf
        nmcmpf = zk8(jcescf+ii-1)
        jj = indik8( zk8(jcesco) , nmcmpf, 1 , nbcmpo )
        if (jj .ne. 0) then
            if (niv .ge. 2) then
                write(ifm,'(A)') 'COMPOSANTE <'//nmcmpf//'>'
                write(ifm,'(A,I5)') '   INDEX DANS CARCOQUF ',ii
                write(ifm,'(A,I5)') '   INDEX DANS CARCOQUE ',jj
            endif
        else
            iret = iret + 1
        endif
100 end do
    ASSERT(iret.eq.0)
!
! --- INFORMATIONS SUR LE MAILLAGE
    call dismoi('NOM_MAILLA', cartco, 'CARTE', repk=nomma)
    call dismoi('NB_MA_MAILLA', nomma, 'MAILLAGE', repi=nbmail)
!
    k24bid = nomma//'.COORDO    .VALE'
    call jeveuo(k24bid, 'L', igeom)
    connex = nomma//'.CONNEX'
    call jeveuo(connex, 'L', jconne)
    call jeveuo(jexatr(connex, 'LONCUM'), 'L', jtabco)
!
    call jeveuo(celscf//'.CESL', 'L', jceslf)
    call jeveuo(celsco//'.CESL', 'L', jceslo)
    call jeveuo(celscf//'.CESV', 'L', jcesvf)
    call jeveuo(celsco//'.CESV', 'E', jcesvo)
! --- TRAITEMENT
!        BOUCLE SUR TOUTES LES MAILLES
!           BOUCLE SUR LES COMPOSANTES AVEC FONCTIONS DE CELSCF
!              SI LA FONCTION EXISTE
!                 CALCUL DE LA POSITION DU CDG DE LA MAILLE
!                 CALCUL DE LA FONCTION
!                 AFFECTE LE RESULTAT A LA COMPOSANTE DE CELSCO
    if (niv .ge. 2) then
        write(ifm,'(A)') 'VALEURS DES FONCTIONS'
        write(ifm,90)
    endif
    do 200 ii = 1, nbmail
        lcoor = .false.
        do 210 jj = 1, nbcmpf
            nmcmpf = zk8(jcescf+jj-1)
            call cesexi('C', jcesdf, jceslf, ii, 1,&
                        1, jj, iad)
            if (iad .gt. 0) then
                nomfct = zk8(jcesvf-1+iad)
                if (nomfct(1:2) .ne. '&&') then
                    if (.not. lcoor) then
                        lcoor = .true.
                        iadr1 = zi(jtabco-1+ii)
                        iadr2 = zi(jtabco-1+ii+1)
                        nbno = iadr2 - iadr1
                        adrm = jconne-1+iadr1
!                    CENTRE DE GRAVITE DE LA MAILLE
                        do 250 icompo = 1, 3
                            valr(icompo) = 0.0d0
                            do 240 inoeu = 1, nbno
                                nunoe = zi(adrm-1+inoeu)
                                valr(icompo) = valr(icompo) + zr( igeom+3*(nunoe-1)+icompo-1)
240                         continue
                            valr(icompo) = valr(icompo)/nbno
250                     continue
                    endif
                    call fointe('F', nomfct, 3, nomval, valr,&
                                fresu, iret)
                    if (niv .ge. 2) then
                        write(ifm,91) ii,jj,(valr(icompo),icompo=1,3),&
                        nomfct,fresu
                    endif
!                 ON RECHERCHE DANS CELSCO LA COMPOSANTE CORRESPONDANTE
                    kk = indik8( zk8(jcesco) , nmcmpf, 1 , nbcmpo )
                    call cesexi('S', jcesdo, jceslo, ii, 1,&
                                1, kk, iad)
                    zr(jcesvo-1+iad) = fresu
                endif
            endif
210     continue
200 end do
!
!     DESTRUCTION DE LA CARTE DES REELS, DES FONCTIONS
    call detrsd('CARTE', cartco)
    call detrsd('CARTE', cartcf)
!     CONSTRUCTION DE LA CARTE DES REELS A PARTIR DE CELSCO
    call cescar(celsco, cartco, 'G')
!     DESTRUCTION DES CHELEM_S
    call detrsd('CHAM_ELEM_S', celsco)
    call detrsd('CHAM_ELEM_S', celscf)
!
    90 format(' MAILLE_NB  : [ [  CENTRE DE GRAVITE ],',&
     &       '  FONCTION  ,  VALEUR       ]')
    91 format("'",i7,"_",i1,"' : [ [",3(e18.10,","),"], '",&
     &       a,"' ,",e18.10,"],")
!
9999 continue
    call jedema()
end subroutine
