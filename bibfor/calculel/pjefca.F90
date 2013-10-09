subroutine pjefca(moa1, lima1, iocc, ncas)
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
! person_in_charge: jacques.pellet at edf.fr
!---------------------------------------------------------------------
! BUT : DETERMINER LE "CAS DE FIGURE" DE LA PROJECTION :
! ----
!  "3D" : ON UTILISE LES MAILLES VOLUMIQUES  : HEXA, ...
!         LE MAILLAGE EST 3D (X,Y,Z)
!  "2D" : ON UTILISE LES MAILLES SURFACIQUES : TRIA, ...
!         LE MAILLAGE EST 2D (X,Y)
!  "2.5D" : ON UTILISE LES MAILLES SURFACIQUES : TRIA, ...
!         MAIS LE MAILLAGE EST 3D (X,Y,Z)
!  "1.5D" : ON UTILISE LES MAILLES LINEIQUES : SEG
!         LE MAILLAGE PEUT ETRE 2D (X,Y) OU 3D (X,Y,Z)
!
! ON ESSAIE DE DETERMINER LE CAS DE FIGURE EN FONCTION
! DES MAILLES DE MOA1.
! MAIS ON SCRUTE AUSSI LE MOT CLE CAS_FIGURE QUI PERMET A L'UTILISATEUR
! DE FORCER CE PARAMETRE.
!
!  ARGUMENTS :
!  -----------
!  IN MOA1 : NOM DU MODELE (OU DU MAILLAGE) CONTENANT LES
!            MAILLES A PROJETER
!  IN IOCC  : 0 OU NUMERO D'OCCURENCE DE VIS_A_VIS
!  IN LIMA1 : NOM DE OBJET JEVEUX CONTENANT LA LISTE DES NUMEROS DE
!             MAILLES A PROJETER (OU ' ' SI IOCC=0).
!  OUT NCAS : CAS DE FIGURE : 3D/2D/2.5D/1.5D
!---------------------------------------------------------------------
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    character(len=*) :: moa1, ncas, lima1
    integer :: iocc
!---------------- VARIABLES LOCALES  --------------------------
    character(len=8) :: moa, nomo1, noma1, cdim1
    integer :: ndim, iagma1, nbma1, ditopo, typm1, dim1, jtypm1
    integer :: nb1, kma, jtmdim, jrepe, n1, iexi
!-----------------------------------------------------------------------
    call jemarq()
!
    moa=moa1
    call jeexin(moa//'.MODELE    .REPE', iexi)
    if (iexi .gt. 0) then
        nomo1=moa
        call dismoi('NOM_MAILLA', nomo1, 'MODELE', repk=noma1)
    else
        nomo1=' '
        noma1=moa
    endif
    call jeveuo(noma1//'.TYPMAIL', 'L', jtypm1)
!
!
!     -- SI L'UTILISATEUR A UTILISE  CAS_FIGURE :
!     -------------------------------------------
    if (iocc .eq. 0) then
        call getvtx(' ', 'CAS_FIGURE', scal=ncas, nbret=n1)
        if (n1 .eq. 1) goto 30
    else
        call getvtx('VIS_A_VIS', 'CAS_FIGURE', iocc=iocc, scal=ncas, nbret=n1)
        if (n1 .eq. 1) goto 30
    endif
!
!
!     CALCUL DE LVAVIS, IAGMA1, NBMA1 (MAILLES CONCERNEES) :
!     ---------------------------------------------------------
    if (iocc .gt. 0) then
        call jeveuo(lima1, 'L', iagma1)
        call jelira(lima1, 'LONMAX', nbma1)
    else
        call dismoi('NB_MA_MAILLA', noma1, 'MAILLAGE', repi=nb1)
        if (nomo1 .ne. ' ') then
            call jeveuo(nomo1//'.MODELE    .REPE', 'L', jrepe)
        endif
!
        call wkvect('&&PJEFCA.LIMA1', 'V V I', nb1, iagma1)
        nbma1=0
        do 10 kma = 1, nb1
            if (nomo1 .ne. ' ') then
!          -- SI C'EST UNE MAILLE DU MODELE :
                if (zi(jrepe-1+2*(kma-1)+1) .gt. 0) then
                    nbma1=nbma1+1
                    zi(iagma1-1+nbma1)=kma
                endif
            else
                nbma1=nbma1+1
                zi(iagma1-1+nbma1)=kma
            endif
 10     continue
    endif
!
!
!     DETERMINATION DE LA DIMENSION DE L'ESPACE (NDIM) :
!     --------------------------------------------------------
    call dismoi('Z_CST', noma1, 'MAILLAGE', repk=cdim1)
    if (cdim1 .eq. 'OUI') then
        ndim=2
    else
        ndim=3
    endif
!
!
!
!     DETERMINATION DU CAS DE FIGURE : 2D, 3D , 2.5D OU 1.5D :
!     --------------------------------------------------------
    call jeveuo('&CATA.TM.TMDIM', 'L', jtmdim)
!
!     -- ON PARCOURT LES MAILLES DE LIMA1 POUR DETERMINER
!        LA PLUS GRANDE DIMENSION TOPOLOGIQUE : 3,2,1 : DITOPO
    ditopo=0
    do 20 kma = 1, nbma1
        typm1=zi(jtypm1-1+zi(iagma1-1+kma))
        dim1=zi(jtmdim-1+typm1)
        ditopo=max(ditopo,dim1)
 20 end do
!
    if (ditopo .eq. 3) then
        ASSERT(ndim.eq.3)
        ncas='3D'
    else if (ditopo.eq.1) then
        ncas='1.5D'
    else if (ditopo.eq.2) then
        if (ndim .eq. 2) then
            ncas='2D'
        else if (ndim.eq.3) then
            ncas='2.5D'
        else
            ASSERT(.false.)
        endif
    else
        ASSERT(.false.)
    endif
!
!
    call jedetr('&&PJEFCA.LIMA1')
!
 30 continue
    call jedema()
end subroutine
