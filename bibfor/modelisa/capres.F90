subroutine capres(char, ligrmo, noma, ndim, fonree)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/char_affe_neum.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/vetyma.h"
#include "asterfort/xtmafi.h"
#include "asterfort/xvelfm.h"
    integer :: ndim
    character(len=4) :: fonree
    character(len=8) :: char, noma
    character(len=*) :: ligrmo
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! BUT : STOCKAGE DES PRESSIONS DANS UNE CARTE ALLOUEE SUR LE
!       LIGREL DU MODELE (Y COMPRIS THM)
!
! ARGUMENTS D'ENTREE:
!      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
!      LIGRMO : NOM DU LIGREL DE MODELE
!      NOMA   : NOM DU MAILLAGE
!      NDIM   : DIMENSION DU PROBLEME (2D OU 3D)
!      FONREE : FONC OU REEL
!-----------------------------------------------------------------------
    integer :: ibid, npres, ncmp, jvalv, jncmp, iocc, np, nc, nbtou, nbma
    integer :: jma, nfiss, nfismx
    parameter    (nfismx=100)
    character(len=8) :: k8b, typmcl(2), fiss(nfismx)
    character(len=16) :: motclf, motcle(2)
    character(len=19) :: carte
    character(len=24) :: mesmai, lismai
    character(len=19) :: cartes(1)
    integer :: ncmps(1)
!-----------------------------------------------------------------------
    call jemarq()
!
    motclf = 'PRES_REP'
    call getfac(motclf, npres)
!
    carte = char//'.CHME.PRESS'
!
    if (fonree .eq. 'REEL') then
        call alcart('G', carte, noma, 'PRES_R')
    else if (fonree.eq.'FONC') then
        call alcart('G', carte, noma, 'PRES_F')
    else
        ASSERT(.false.)
    endif
!
    call jeveuo(carte//'.NCMP', 'E', jncmp)
    call jeveuo(carte//'.VALV', 'E', jvalv)
!
! --- STOCKAGE DE FORCES NULLES SUR TOUT LE MAILLAGE
!
    ncmp = 2
    zk8(jncmp) = 'PRES'
    zk8(jncmp+1) = 'CISA'
!
    if (fonree .eq. 'REEL') then
        zr(jvalv) = 0.d0
        zr(jvalv+1) = 0.d0
    else
        zk8(jvalv) = '&FOZERO'
        zk8(jvalv+1) = '&FOZERO'
    endif
    call nocart(carte, 1, ' ', 'NOM', 0,&
                ' ', 0, ligrmo, ncmp)
!
    mesmai = '&&CAPRES.MES_MAILLES'
    lismai = '&&CAPRES.NUM_MAILLES'
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
! --- STOCKAGE DANS LA CARTE
!
    do iocc = 1, npres
!
        if (fonree .eq. 'REEL') then
            call getvr8(motclf, 'PRES', iocc=iocc, scal=zr(jvalv), nbret=np)
            call getvr8(motclf, 'CISA_2D', iocc=iocc, scal=zr(jvalv+1), nbret=nc)
        else
            call getvid(motclf, 'PRES', iocc=iocc, scal=zk8(jvalv), nbret=np)
            call getvid(motclf, 'CISA_2D', iocc=iocc, scal=zk8(jvalv+1), nbret=nc)
        endif
        if (nc .ne. 0 .and. ndim .eq. 3) then
            call utmess('F', 'MODELISA9_94')
        endif
!
        call getvtx(motclf, 'TOUT', iocc=iocc, scal=k8b, nbret=nbtou)
        call getvid(motclf, 'FISSURE', iocc=iocc, nbval=0, nbret=nfiss)
!
        if (nfiss .ne. 0) then
!
!           PAS DE CISA_2D SUR LES LÈVRES DES FISSURES X-FEM
            if (nc .ne. 0) then
                call utmess('F', 'XFEM_14')
            endif
!
            nfiss = -nfiss
            call getvid(motclf, 'FISSURE', iocc=iocc, nbval=nfiss, vect=fiss,&
                        nbret=ibid)
!           VERIFICATION DE LA COHERENCE ENTRE LES FISSURES ET LE MODELE
            call xvelfm(nfiss, fiss, ligrmo(1:8))
!           RECUPERATION DES MAILLES PRINCIPALES X-FEM FISSUREES
            call xtmafi(noma, ndim, fiss, nfiss, lismai,&
                        mesmai, nbma)
            call jeveuo(mesmai, 'L', jma)
            call nocart(carte, 3, k8b, 'NOM', nbma,&
                        zk8(jma), ibid, ' ', ncmp)
            call jedetr(mesmai)
            call jedetr(lismai)
!
        else
!
            cartes(1) = carte
            ncmps(1) = ncmp
            call char_affe_neum(noma, ndim, motclf, iocc, 1,&
                                cartes, ncmps)
        endif
!
    end do
!
!-----------------------------------------------------------------------
    call jedema()
end subroutine
