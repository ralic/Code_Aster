subroutine affdef(tmp, nom, nel, ntel, tab,&
                  ier)
    implicit none
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
!
    integer :: ntel(*)
    character(len=8) :: tab(*)
    character(len=24) :: nom, tmp
!       ----------------------------------------------------------------
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
!  - VERIFICATION DE LA COMPLETUDE DES DONNES OBLIGATOIRES A
!    ENTRER , ET DE LA POSITIVITE DE CERTAINES VALEURS
!  - AFFECTATION DES VALEURS PAR DEFAUT AUX CARACTERISTIQUES
!    GENERALES ET GEOMETRIQUES ABSENTES
!  - NTEL(I)  = NUMERO DU TYPE ELEMENT
!       I = 1 : MECA_POU_D_T
!           2 : MECA_POU_D_E
!           4 : MECA_POU_C_T
!           5 : MEFS_POU_D_T
!           6 : MECA_POU_D_TG
!          12 : MECA_POU_D_EM
!          13 : MECA_POU_D_TGM
!
! --- ------------------------------------------------------------------
!     TAB  1      2      3      4      5     6     7     8    9    10
!     0    A1     IY1    IZ1    AY1    AZ1   EY1   EZ1   JX1  RY1  RZ1
!     1    RT1    A2     IY2    IZ2    AY2   AZ2   EY2   EZ2  JX2  RY2
!     2    RZ2    RT2    TVAR   HY1    HZ1   EPY1  EPZ1  HY2  HZ2  EPY2
!     3    EPZ2   R1     E1     R2     E2    TSEC  AI1   AI2  JG1  JG2
!     4    IYR21  IYR22  IZR21  IZR22
!--- -------------------------------------------------------------------
    integer :: ier, isec, j, jdge, nc, nd, ne
    integer :: nel, ng, nr, nt, nx, ny, nz
!--- -------------------------------------------------------------------
    parameter       ( nr = 4 , nc = 2, ng = 8 )
    parameter       ( nt = 4 , ne =12, nd = 6 )
    parameter       ( nx = 10, ny = 8, nz = 4 )
    integer :: ogen(ng), orec(nr), ocer(nc), otpe(nt)
    integer :: dexc(ne), ddfx(nd), drec(nr), dcer(nc)
    integer :: pgen(nx), prec(ny), pcer(nz)
    character(len=24) :: valk(3)
    real(kind=8) :: tst
!--- -------------------------------------------------------------------
    data ogen      /1,2,3,8,12,13,14,19/
    data orec      /24,25,28,29/
    data ocer      /32,34/
    data otpe      /4,5,15,16/
    data dexc      /6,7,17,18,37,38,39,40,41,42,43,44/
    data ddfx      /9,10,11,20,21,22/
    data drec      /26,27,30,31/
    data dcer      /33,35/
    data pgen      /1,9,10,11,12,20,21,22,37,38/
    data prec      /24,25,26,27,28,29,30,31/
    data pcer      /32,33,34,35/
!--- -------------------------------------------------------------------
!
    call jemarq()
    tst = r8maem()
!
    call jeveuo(jexnom(tmp, nom), 'E', jdge)
    isec = nint(zr(jdge+35))
!
!     COMPLETUDE DES DONNES GENERALES
!
    if (isec .eq. 0) then
        do j = 1, ng
            if (zr(jdge+ogen(j)-1) .eq. tst) then
                valk(1) = nom
                valk(2) = tab(ogen(j))
                call utmess('A', 'MODELISA_77', nk=2, valk=valk)
                ier = ier + 1
            endif
        enddo
!        1:MECA_POU_D_T     4:MECA_POU_C_T
!        5:MEFS_POU_D_T     6:MECA_POU_D_TG
!       13:MECA_POU_D_TGM
        if ((nel.eq.ntel(1) ) .or. (nel.eq.ntel(4) ) .or. (nel.eq.ntel(5) ) .or.&
            (nel.eq.ntel(6) ) .or. (nel.eq.ntel(13))) then
            do j = 1, nt
                if (zr(jdge+otpe(j)-1) .eq. tst) then
                    valk(1) = nom
                    valk(2) = tab(otpe(j))
                    call utmess('A', 'MODELISA_78', nk=2, valk=valk)
                endif
            enddo
        endif
    endif
!
!     COMPLETUDE DES DONNES GEOMETRIQUES RECTANGLE
!
    if (isec .eq. 1) then
        do j = 1, nr
            if (zr(jdge+orec(j)-1) .eq. tst) then
                valk(1) = nom
                valk(2) = tab(orec(j))
                call utmess('A', 'MODELISA_79', nk=2, valk=valk)
                ier = ier + 1
            endif
        enddo
    endif
!
!     COMPLETUDE DES DONNES GEOMETRIQUES CERCLE
!
    if (isec .eq. 2) then
        do j = 1, nc
            if (zr(jdge+ocer(j)-1) .eq. tst) then
                valk(1) = nom
                valk(2) = tab(ocer(j))
                call utmess('A', 'MODELISA_80', nk=2, valk=valk)
                ier = ier + 1
            endif
        enddo
    endif
!
!     VERIFICATION DE LA STRICTE POSITIVITE DE  VALEURS GENERALE
!
    if (isec .eq. 0) then
        do j = 1, nx
            if (zr(jdge+pgen(j)-1) .ne. tst) then
                if (zr(jdge+pgen(j)-1) .le. 0.d0) then
                    valk(1) = nom
                    valk(2) = tab(pgen(j))
                    call utmess('A', 'MODELISA_81', nk=2, valk=valk)
                    ier = ier + 1
                endif
            endif
        enddo
    endif
!
!     VERIFICATION DE LA STRICTE POSITIVITE DE VALEURS RECTANGLE
!
    if (isec .eq. 1) then
        do j = 1, ny
            if (zr(jdge+prec(j)-1) .ne. tst) then
                if (zr(jdge+prec(j)-1) .le. 0.d0) then
                    valk(1) = nom
                    valk(2) = tab(prec(j))
                    call utmess('A', 'MODELISA_82', nk=2, valk=valk)
                    ier = ier + 1
                endif
            endif
        enddo
    endif
!
!     VERIFICATION DE LA STRICTE POSITIVITE DE VALEURS CERCLE
!
    if (isec .eq. 2) then
        do j = 1, nz
            if (zr(jdge+pcer(j)-1) .ne. tst) then
                if (zr(jdge+pcer(j)-1) .le. 0.d0) then
                    valk(1) = nom
                    valk(2) = tab(pcer(j))
                    call utmess('A', 'MODELISA_83', nk=2, valk=valk)
                    ier = ier + 1
                endif
            endif
        enddo
    endif
!
    if (ier .ne. 0) goto 999
!
!     AFFECTATION DES VALEURS PAR DEFAUT POUR LES DONNEES GENERALES
!
    if (isec .eq. 0) then
!        EXCENTREMENTS, AIRES INTERIEURES, CONSTANTES DE GAUCHISSEMENT
        do j = 1, ne
            if(zr(jdge+dexc(j)-1).eq.tst)zr(jdge+dexc(j)-1) = 0.d0
        enddo
!        DIST. FIBRE EXT.+ RAYON TORSION
        do j = 1, nd
            if(zr(jdge+ddfx(j)-1).eq.tst)zr(jdge+ddfx(j)-1) = 1.d0
        enddo
!        EULER
        if (nel .eq. ntel(2) .or. nel .eq. ntel(12)) then
            do j = 1, nt
                zr(jdge+otpe(j)-1) = 0.d0
            enddo
        endif
    endif
!
!     AFFECTATION DES VALEURS PAR DEFAUT POUR LES DONNEES RECTANGLE
!
    if (isec .eq. 1) then
        do j = 1, nr
            if (zr(jdge+drec(j)-1) .eq. tst) then
                zr(jdge+drec(j)-1) = zr(jdge+orec(j)-1) / 2.d0
            else
                if (zr(jdge+drec(j)-1) .gt. (zr(jdge+orec(j)-1)/2.d0)) then
                    valk(1) = nom
                    valk(2) = tab(drec(j))
                    valk(3) = tab(orec(j))
                    call utmess('A', 'MODELISA_84', nk=3, valk=valk)
                    ier = ier + 1
                endif
            endif
        enddo
    endif
!
!     AFFECTATION DES VALEURS PAR DEFAUT POUR LES DONNEES CERCLE
!
    if (isec .eq. 2) then
        do j = 1, nc
            if (zr(jdge+dcer(j)-1) .eq. tst) then
                zr(jdge+dcer(j)-1) = zr(jdge+ocer(j)-1)
            else
                if (zr(jdge+dcer(j)-1) .gt. zr(jdge+ocer(j)-1)) then
                    valk(1) = nom
                    valk(2) = tab(dcer(j))
                    valk(3) = tab(ocer(j))
                    call utmess('A', 'MODELISA_85', nk=3, valk=valk)
                    ier = ier + 1
                endif
            endif
        enddo
    endif
!
999  continue
    call jedema()
end subroutine
