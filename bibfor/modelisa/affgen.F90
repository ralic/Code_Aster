subroutine affgen(tmp, nom, nel, ntel, napcis,&
                  foncis)
    implicit none
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/assert.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
!
    integer :: ntel(*)
    character(len=19) :: napcis, foncis
    character(len=24) :: tmp, nom
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!       AFFECTATION DES CARACTERISTIQUES GENERALES CALCULEES
!       A PARTIR DES DONNEES GEOMETRIQUES (RECTANGLE ET CERCLE)
!       RQ : NTEL(1) = NUMERO DU TYPE ELEMENT MECA_POU_D_T
!            NTEL(2) = NUMERO DU TYPE ELEMENT MECA_POU_D_E
!            NTEL(4) = NUMERO DU TYPE ELEMENT MECA_POU_C_T
!            NTEL(5) = NUMERO DU TYPE ELEMENT MEFS_POU_D_T
!            NTEL(6) = NUMERO DU TYPE ELEMENT MECA_POU_D_TG
!     ------------------------------------------------------------------
    real(kind=8) :: eps, pi, alpha, beta, ccis
    real(kind=8) :: hy, hz, epy, epz, hyi, hzi
    real(kind=8) :: a, b, a4, b4, b3
    real(kind=8) :: ct, cd, jx
    real(kind=8) :: re, ri, e
    real(kind=8) :: valpay(2), valpaz(2), valpaf
    character(len=24) :: nompa(2), nompaf
!-----------------------------------------------------------------------
    integer :: i, ier, igen, igen2, igeoc, igeor, isec
    integer :: jdge, nel
    real(kind=8) :: aint, ay, az
!-----------------------------------------------------------------------
    data    eps     /1.d-3/
!     ------------------------------------------------------------------
!
    call jemarq()
    pi = r8pi()
!
!
    if (.not.(&
        nel .eq. ntel(1) .or. nel .eq. ntel(4) .or. nel .eq. ntel(2) .or. nel .eq. ntel(3)&
        .or. nel .eq. ntel(5) .or. nel .eq. ntel(6) .or. nel .eq. ntel(12) .or. nel .eq.&
        ntel(13) .or. nel .eq. ntel(9) .or. nel .eq. ntel(10) .or. nel .eq. ntel(11) .or.&
        nel .eq. ntel(7) .or. nel .eq. ntel(8)&
        )) then
        call utmess('F', 'MODELISA_86')
    endif
!
!
    call jeveuo(jexnom(tmp, nom), 'E', jdge)
    isec = nint(zr(jdge+35))
!
!
! ---   CALCUL DES CARACTERISTIQUES GENERALES SECTION RECTANGULAIRE
!
!       -  ERREUR   SI  HY  <= 0  OU  HZ  <= 0  OU
!                       EPY <= 0  OU  EPZ <= 0  (TEST DANS AFFDEF)
!
    if (isec .eq. 1) then
        do 10 i = 1, 2
            if (i .eq. 1) then
                igeor = 24
                igeoc = 32
                igen = 1
                igen2 = 37
            else
                igeor = 28
                igeoc = 34
                igen = 12
                igen2 = 38
            endif
            zr(jdge+igeoc-1) = 0.d0
            zr(jdge+igeoc) = 0.d0
            hy = zr(jdge+igeor-1)
            hz = zr(jdge+igeor)
            epy = zr(jdge+igeor+1)
            epz = zr(jdge+igeor+2)
            hyi = hy - 2.d0*epy
            hzi = hz - 2.d0*epz
!  A
            zr(jdge+igen-1) = hy * hz - hyi * hzi
!  IY
            zr(jdge+igen) = (hy*(hz*hz*hz)-hyi*(hzi*hzi*hzi))/12.d0
!  IZ
            zr(jdge+igen+1) = (hz*(hy*hy*hy)-hzi*(hyi*hyi*hyi))/12.d0
!  EY
            zr(jdge+igen+4) = 0.d0
!  EZ
            zr(jdge+igen+5) = 0.d0
!  RY
            zr(jdge+igen+7) = hy / 2.d0
!  RZ
            zr(jdge+igen+8) = hz / 2.d0
!
!           --- CAS DE LA SECTION  RECTANGULAIRE  PLEINE ---
            if (abs(hyi/hy) .le. eps .or. abs(hzi/hz) .le. eps) then
                a = hy / 2.d0
                b = hz / 2.d0
                if (a/b .lt. 1.d0) then
                    a = hz / 2.d0
                    b = hy / 2.d0
                endif
                a4 = a**4 * 12.d0
                b4 = b**4
                b3 = b**3
                jx = a*b3*(16.d0/3.d0-3.36d0*b*(1.d0-b4/a4)/a)
!  AY
                if (nel .eq. ntel(1)) zr(jdge+igen+2) = 1.2d0
                if (nel .eq. ntel(2)) zr(jdge+igen+2) = 0.d0
                if (nel .eq. ntel(3)) zr(jdge+igen+2) = 1.2d0
                if (nel .eq. ntel(4)) zr(jdge+igen+2) = 1.2d0
                if (nel .eq. ntel(5)) zr(jdge+igen+2) = 1.2d0
                if (nel .eq. ntel(6)) zr(jdge+igen+2) = 1.2d0
                if (nel .eq. ntel(12)) zr(jdge+igen+2) = 0.d0
                if (nel .eq. ntel(13)) zr(jdge+igen+2) = 1.2d0
!  AZ
                if (nel .eq. ntel(1)) zr(jdge+igen+3) = 1.2d0
                if (nel .eq. ntel(2)) zr(jdge+igen+3) = 0.d0
                if (nel .eq. ntel(3)) zr(jdge+igen+3) = 1.2d0
                if (nel .eq. ntel(4)) zr(jdge+igen+3) = 1.2d0
                if (nel .eq. ntel(5)) zr(jdge+igen+3) = 1.2d0
                if (nel .eq. ntel(6)) zr(jdge+igen+3) = 1.2d0
                if (nel .eq. ntel(12)) zr(jdge+igen+3) = 0.d0
                if (nel .eq. ntel(13)) zr(jdge+igen+3) = 1.2d0
!  JX
                zr(jdge+igen+6) = jx
!  RT
                zr(jdge+igen+9) = jx * (3.d0*a+1.8d0*b) / (8.d0*a*a*b* b)
!  AI
                zr(jdge+igen2-1) = 0.d0
!
            else
!
!              --- CAS DU TUBE RECTANGULAIRE ---
                ct = 2.d0*epy*epz*(hy-epy)*(hy-epy)*(hz-epz)*(hz-epz)
                cd = hy*epy + hz*epz - epy*epy - epz*epz
                jx = ct /cd
!
!          --- INTERPOLATION DES COEFFICIENTS DE CISAILLEMENT
                alpha = (hy - 2.d0 * epy ) / hy
                beta = (hz - 2.d0 * epz ) / hz
                ASSERT((alpha.ge.0.d0) .or. (beta.ge.0.d0))
                if (alpha .gt. 0.95d0 .or. beta .gt. 0.95d0) then
                    call utmess('F', 'MODELISA10_15')
                endif
                nompa(1)= 'ALPHA'
                nompa(2)= 'BETA'
                valpay(1)= alpha
                valpay(2)= beta
                valpaz(1)= beta
                valpaz(2)= alpha
                call fointe('A', napcis, 2, nompa, valpay,&
                            ay, ier)
                call fointe('A', napcis, 2, nompa, valpaz,&
                            az, ier)
!
!  AY
                if (nel .eq. ntel(2) .or. nel .eq. ntel(12)) then
                    zr(jdge+igen+2) = 0.d0
                else
                    zr(jdge+igen+2) = ay
                endif
!  AZ
                if (nel .eq. ntel(2) .or. nel .eq. ntel(12)) then
                    zr(jdge+igen+3) = 0.d0
                else
                    zr(jdge+igen+3) = az
                endif
!  JX
                zr(jdge+igen+6) = jx
!  RT. TUBE RECTANGULAIRE MINCE D'EPAISSEUR CONSTANTE. RT=JX/2.E.AINT
                aint = (hy-2.d0*epy)*(hz-2.d0*epz)
                zr(jdge+igen+9) = jx/(2.d0*epz*aint)
!  AI
                zr(jdge+igen2-1) = hyi * hzi
!
            endif
! AY/AZ POUR TUYAUX ET 3D_FAISCEAU
            if (nel .eq. ntel(9) .or. nel .eq. ntel(10) .or. nel .eq. ntel(11) .or. nel&
                .eq. ntel(7) .or. nel .eq. ntel(8)) then
                zr(jdge+igen+2) = 0.d0
                zr(jdge+igen+3) = 0.d0
            endif
!
10      continue
!  JG1,JG2,IYR21,IYR22,IZR21,IZR22 :
        do 11 i = 1, 6
            zr(jdge-1+38+i) = 0.d0
11      continue
    endif
!
! ---   CALCUL DES CARACTERISTIQUES GENERALES SECTION CIRCULAIRE
!
!       -  ERREUR   SI RE <= 0  OU  E > RE OU E <= 0  (TEST DANS AFFDEF)
!
    if (isec .eq. 2) then
        do 20 i = 1, 2
            if (i .eq. 1) then
                igeor = 24
                igeoc = 32
                igen = 1
                igen2 = 37
            else
                igeor = 28
                igeoc = 34
                igen = 12
                igen2 = 38
            endif
            zr(jdge+igeor-1) = 0.d0
            zr(jdge+igeor) = 0.d0
            zr(jdge+igeor+1) = 0.d0
            zr(jdge+igeor+2) = 0.d0
            re = zr(jdge+igeoc-1)
            e = zr(jdge+igeoc)
            ri = re - e
!  A
            zr(jdge+igen-1) = pi * ( re*re - ri*ri )
!  IY
            zr(jdge+igen) = pi * ( re**4 - ri**4 ) / 4.d0
!  IZ
            zr(jdge+igen+1) = zr(jdge+igen)
!  EY
            zr(jdge+igen+4) = 0.d0
!  EZ
            zr(jdge+igen+5) = 0.d0
!  JX
            zr(jdge+igen+6) = zr(jdge+igen) * 2.d0
!  RY
            zr(jdge+igen+7) = re
!  RZ
            zr(jdge+igen+8) = re
!  RT
            zr(jdge+igen+9) = re
!
!       --- INTERPOLATION DES COEFFICIENTS DE CISAILLEMENT
!
            alpha = ri / re
            ASSERT((alpha .ge. 0.d0) .or. (alpha .le. 1.d0))
            nompaf = 'ALPHA'
            valpaf = alpha
            call fointe('A', foncis, 1, [nompaf], [valpaf],&
                        ccis, ier)
!  AY
            if (nel .eq. ntel(2) .or. nel .eq. ntel(12)) then
                zr(jdge+igen+2) = 0.d0
            else
                zr(jdge+igen+2) = ccis
            endif
!  AZ
            if (nel .eq. ntel(2) .or. nel .eq. ntel(12)) then
                zr(jdge+igen+3) = zr(jdge+igen+2)
            else
                zr(jdge+igen+3) = zr(jdge+igen+2)
            endif
!  AI
            zr(jdge+igen2-1) = pi * ri * ri
! AY/AZ POUR TUYAUX ET 3D_FAISCEAU
            if (nel .eq. ntel(9) .or. nel .eq. ntel(10) .or. nel .eq. ntel(11) .or. nel&
                .eq. ntel(7) .or. nel .eq. ntel(8)) then
                zr(jdge+igen+2) = 0.d0
                zr(jdge+igen+3) = 0.d0
            endif
!
20      continue
!  JG1,JG2,IYR21,IYR22,IZR21,IZR22 :
        do 21 i = 1, 6
            zr(jdge-1+38+i) = 0.d0
21      continue
    endif
!
    call jedema()
end subroutine
