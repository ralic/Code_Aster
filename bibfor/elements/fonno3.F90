subroutine fonno3(noma, tablev, ndim, na, nb,&
                  noe)
    implicit none
#include "jeveux.h"
#include "asterfort/confac.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
!
    character(len=8) :: noma
    integer :: tablev(2), ndim, na, nb, noe(4, 4)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       ----------------------------------------------------------------
!      RECUP DES FACES CONNECTEES AU FOND
!          POUR CHACUNE DES 2 MAILLES
!       ----------------------------------------------------
!    ENTREES
!       NOMA   : NOM DU MAILLAGE
!       TABLEV : VECTEUR CONTNANT LES NUMEROS DES DEUX MAILLES
!                CONNECTEES AU NOEUD SOMMET COURANT ET AUX LEVRES
!       NDIM   : DIMENSION DU MODELE
!       NA     : NUMERO DU NOEUD SOMMET COURANT
!       NB     : NUMERO DU NOEUD SOMMET SUIVANT
!    SORTIE
!       NOE    : NOEUDS DES FACES CONTENANT NA et NB ET APPARTENANT AUX
!                MAILLES CONNECTEES AU NOEUD SOMMET COURANT
!                ET AUX LEVRES
!
    integer :: iatyma, iamase, ityp
    integer :: i, j, jf, numert(12, 3), nbft, numero(6, 8), nbf
    integer :: compte, ima, nn, inp, compt(2), compf
    integer :: numerf(4, 2)
    character(len=8) :: type
!
!     -----------------------------------------------------------------
!
    call jemarq()
!
!
!     RECUPERATION DE L'ADRESSE DES TYPFON DE MAILLES
    call jeveuo(noma//'.TYPMAIL', 'L', iatyma)
!
    compte=0
    do i = 1, 4
        do j = 1, 4
            noe(i,j)=0
        end do
    end do
    do ima = 1, 2
        ityp = iatyma-1+tablev(ima)
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(ityp)), type)
        call dismoi('NBNO_TYPMAIL', type, 'TYPE_MAILLE', repi=nn)
        call jeveuo(jexnum( noma//'.CONNEX', tablev(ima)), 'L', iamase)
!
!       EN 3D
!
        if (ndim .eq. 3) then
            call confac(type, numert, nbft, numero, nbf)
!         RECHERCHE DES INDICES LOCAUX
            i = 1
            do inp = 1, nn
                if ((zi(iamase-1+inp).eq.na) .or. (zi(iamase-1+inp) .eq.nb)) then
                    compt(i) = inp
                    i = i+1
                endif
            end do
!         RECHERCHE DE LA FACE
            do inp = 1, nbf
                compf = 0
                do i = 1, 4
                    if ((numero(inp,i).eq.compt(1)) .or. (numero(inp,i) .eq.compt(2))) then
                        compf = compf + 1
                    endif
                end do
                if (compf .eq. 2) then
!             RECUPERATION DES NOEUDS SOMMETS DE LA FACE INP
                    compte = compte + 1
                    do jf = 1, 4
                        if (numero(inp,jf) .ne. 0) then
                            noe(compte,jf) = zi(iamase-1+numero(inp, jf))
                        else
                            noe(compte,jf) = 0
                        endif
                    end do
                endif
            end do
!
!
!       EN 2D
!
        else if (ndim.eq.2) then
            if (type(1:4) .eq. 'QUAD') then
                numerf(1,1)=1
                numerf(1,2)=2
                numerf(2,1)=2
                numerf(2,2)=3
                numerf(3,1)=3
                numerf(3,2)=4
                numerf(4,1)=4
                numerf(4,2)=1
            else
                numerf(1,1)=1
                numerf(1,2)=2
                numerf(2,1)=2
                numerf(2,2)=3
                numerf(3,1)=3
                numerf(3,2)=1
                numerf(4,1)=0
                numerf(4,2)=0
            endif
!         RECHERCHE DES INDICES LOCAUX
            i = 1
            do inp = 1, nn
                if (zi(iamase-1+inp) .eq. na) then
                    compt(i) = inp
                    i = i+1
                endif
            end do
!         RECHERCHE DE LA FACE OU ARETE
            do inp = 1, 4
                compf = 0
                do i = 1, 2
                    if (numerf(inp,i) .eq. compt(1)) then
                        compf = compf + 1
                    endif
                end do
                if (compf .eq. 1) then
!             RECUPERATION DES NOEUDS SOMMETS DE LA FACE INP
                    compte = compte + 1
                    do jf = 1, 2
                        noe(compte,jf) = zi(iamase-1+numerf(inp,jf))
                    end do
                endif
            end do
        endif
    end do
!
    call jedema()
end subroutine
