subroutine coefam(ipas, ires, x, xsi0, cd)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!   CALCUL DU COEFFICIENT D AMORTISSEMENT AJOUTE CD EN FONCTION
!   DE LA VITESSE REDUITE (FAISCEAU DE TUBES SOUS ECOULEMENT TRANSVERSE)
!-----------------------------------------------------------------------
! aslint: disable=W1501
    implicit none
!  IN    : IPAS      : TYPE DE PAS
!  IN    : IRES      : TYPE DE RESEAU DU POINT COURANT
!  IN    : X         : VITESSE REDUITE
!  OUT   : CD        : COEFFICIENT D AMORTISSEMENT AJOUTE
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/cdatrc.h"
#include "asterfort/coefal.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    integer :: ipas, ires
    real(kind=8) :: cd, xsi0
!
    integer :: nborcd, ncdmax, iret
    integer :: jborne, jcoeff, jvired
    real(kind=8) :: zero, borncd(20), coefcd(20, 11)
    character(len=24) :: nom1, nom2, nom3
!
! ----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ipas1, ires1, j, k
    real(kind=8) :: x
!-----------------------------------------------------------------------
    call jemarq()
!
    x = dble(abs(x))
!
    ncdmax = 11
    zero = 0.0d0
!
    nom1 = '&&COEFAM.CDI'
    nom2 = '&&COEFAM.CDR1'
    nom3 = '&&COEFAM.CDR2'
!
    if (ires .eq. 0) then
        cd = zero
        goto 1000
    endif
!
! --- ON TESTE L'EXISTENCE DU VECTEUR DE COEFFICIENTS
!     POUR LA CORRELATION RELATIVE A IPAS ET IRES
!     ===============================================
    call jeexin(nom2, iret)
    if (iret .eq. 0) then
!
! --- LECTURE DU FICHIER DE DONNEES
!     =============================
        call coefal(nom1, nom2, nom3, ncdmax, ipas,&
                    ires, borncd, nborcd, coefcd, ipas1,&
                    ires1)
    else
        call jeveuo(nom1, 'L', jborne)
        call jeveuo(nom2, 'L', jcoeff)
        call jeveuo(nom3, 'L', jvired)
        ipas1 = zi(jborne-1+1)
        ires1 = zi(jborne-1+2)
        nborcd = zi(jborne-1+3)
        if (ipas1 .eq. ipas .and. ires1 .eq. ires) then
            k = 1
            do 100 i = 1, nborcd
                borncd(i) = zr(jcoeff + i - 1)
                do 110 j = 1, ncdmax
                    coefcd(i,j) = zr(jcoeff + nborcd + k - 1 )
                    k = k + 1
110              continue
100          continue
        else
            call jedetr(nom1)
            call jedetr(nom2)
            call jedetr(nom3)
            call coefal(nom1, nom2, nom3, ncdmax, ipas,&
                        ires, borncd, nborcd, coefcd, ipas1,&
                        ires1)
        endif
    endif
    if (ipas1 .ne. ipas .or. ires1 .ne. ires) then
        call utmess('F', 'MODELISA4_29')
    endif
!
! **********************************************************************
! ***                  FAISCEAU EN PAS CARRE LIGNE                   ***
! **********************************************************************
!
!
    if (ipas .eq. 1) then
!
        if (ires .ge. 1 .and. ires .le. 1000) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else
                if (x .lt. borncd(nborcd)) then
                    do 130 i = 2, nborcd
                        if (x .ge. borncd(i-1) .and. x .lt. borncd(i)) then
                            cd = coefcd(i-1,1)/(x*x*x*x*x*x*x) + coefcd(i-1,2)/(x*x*x*x*x*x) + co&
                                 &efcd(i-1, 3)/(x*x*x*x*x) + coefcd(i-1,4)/(x*x*x*x) + coefcd(i-1&
                                 &,5)/(x*x*x) + coefcd(i-1,6)/(x* x) + coefcd(i-1,7)/(x) + coefcd&
                                 &(i-1,8) + coefcd(i-1,9)*(x) + coefcd(i-1,10)*(x*x) + coefcd(i-1&
                                 &,11)*(x*x*x)
                            goto 140
                        endif
130                  continue
140                  continue
                else
                    cd = coefcd(nborcd,1)/(x*x*x*x*x*x*x) + coefcd( nborcd,2)/(x*x*x*x*x*x) + coe&
                         &fcd(nborcd,3)/(x*x*x* x*x) + coefcd(nborcd,4)/(x*x*x*x) + coefcd(nborcd&
                         &, 5)/(x*x*x) + coefcd(nborcd,6)/(x*x) + coefcd( nborcd,7)/(x) + coefcd(&
                         &nborcd,8) + coefcd(nborcd, 9)*(x) + coefcd(nborcd,10)*(x*x) + coefcd(nb&
                         &orcd, 11)*(x*x*x)
                endif
            endif
!
!
! --- CELLULE DE TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE1.
!
        else if (ires.eq.1001) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,8) + coefcd(1,9)*x
            else if (x .lt. borncd(3)) then
                cd = coefcd(2,8) + coefcd(2,9)*x
            else if (x .lt. borncd(4)) then
                cd = coefcd(3,8) + coefcd(3,7)/x + coefcd(3,6)/(x*x)
            else if (x .lt. borncd(5)) then
                cd = coefcd(4,8)
            else if (x .lt. borncd(6)) then
                cd = coefcd(5,11)*x*x*x + coefcd(5,10)*x*x + coefcd(5, 9)*x + coefcd(5,8)
            else
                cd = coefcd(6,9)*x
            endif
!
! --- CELLULE DE TUBES VIBRANTS EN MILIEU DE FAISCEAU CLOTAIRE.
!     (PROFIL DE VITESSE REEL)
!
        else if (ires .eq. 1002) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,9)*x + coefcd(1,8)
            else if (x .lt. borncd(3)) then
                cd = coefcd(2,8) + coefcd(2,7)/x + coefcd(2,6)/(x*x) + coefcd(2,5)/(x*x*x)
            else if (x .lt. borncd(4)) then
                cd = coefcd(3,9)*x + coefcd(3,8)
            else if (x .lt. borncd(5)) then
                cd = coefcd(4,8)
            else
                cd = coefcd(5,9)*x
            endif
!
! --- CELLULE DE TUBES VIBRANTS EN MILIEU DE FAISCEAU CLOTAIRE.
!     (PROFIL DE VITESSE UNIFORME)
!
        else if (ires .eq. 1003) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,9)*x + coefcd(1,8)
            else if (x .lt. borncd(3)) then
                cd = coefcd(2,8) + coefcd(2,7)/x + coefcd(2,6)/(x*x) + coefcd(2,5)/(x*x*x)
            else if (x .lt. borncd(4)) then
                cd = coefcd(3,9)*x + coefcd(3,8)
            else if (x .lt. borncd(5)) then
                cd = coefcd(4,8)
            else
                cd = coefcd(5,9)*x
            endif
!
! --- TUBE UNIQUE VIBRANT EN MILIEU DE FAISCEAU RIGIDE VISCACHE1.
!
        else if (ires .eq. 1004) then
!
            if (x .lt. borncd(1)) then
                cd = 0.0d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,9)*x + coefcd(1,8)
            else if (x .lt. borncd(3)) then
                cd = coefcd(2,8) + coefcd(2,7)/x + coefcd(2,6)/(x*x) + coefcd(2,5)/(x*x*x) + coef&
                     &cd(2,4)/(x*x*x*x)
            else
                cd = coefcd(3,8) * (x ** 2.1337d0)
            endif
!
! --- TUBE UNIQUE VIBRANT EN DEBUT DE FAISCEAU RIGIDE VISCACHE1.
!
        else if (ires .eq. 1005) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,9)*x + coefcd(1,8)
            else if (x .lt. borncd(3)) then
                cd = coefcd(2,8) + coefcd(2,7)/x + coefcd(2,6)/(x*x) + coefcd(2,5)/(x*x*x)
            else if (x .lt. borncd(4)) then
                cd = coefcd(3,9)*x + coefcd(3,8)
            else if (x .lt. borncd(5)) then
                cd = coefcd(4,8) + coefcd(4,7)/x + coefcd(4,6)/(x*x) + coefcd(4,5)/(x*x*x)
            else if (x .lt. borncd(6)) then
                cd = coefcd(5,8) + coefcd(5,7)/x + coefcd(5,6)/(x*x)
            else
                cd = coefcd(6,8) * (x ** 1.539d0)
            endif
!
! --- TUBE ROMPU
!
        else if (ires .eq. 1006) then
!
            if (x .le. borncd(1)) then
                cd = 0.d0
            else
                call cdatrc(x, xsi0, coefcd, cd)
            endif
!
! --- TUBE UNIQUE VIBRANT EN MILIEU DE FAISCEAU RIGIDE TANAKA.
!
        else if (ires .eq. 1007) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else
                cd = coefcd(1,1)/(x*x*x*x*x*x*x) + coefcd(1,2)/(x*x*x* x*x*x) + coefcd(1,3)/(x*x*&
                     &x*x*x) + coefcd(1,4)/(x*x*x* x) + coefcd(1,5)/(x*x*x) + coefcd(1,6)/(x*x) +&
                     & coefcd(1,7)/x + coefcd(1,8) + coefcd(1,9)*x
            endif
!
! --- TUBE UNIQUE VIBRANT EN MILIEU DE FAISCEAU RIGIDE DIVA EAU.
!
        else if (ires .eq. 1008) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,9)*x + coefcd(1,8)
            else if (x .lt. borncd(3)) then
                cd = coefcd(2,8) + coefcd(2,7)/x + coefcd(2,6)/(x*x)
            else if (x .lt. borncd(4)) then
                cd = coefcd(3,8) + coefcd(3,7)/x + coefcd(3,6)/(x*x) + coefcd(3,5)/(x*x*x)
            else if (x .lt. borncd(5)) then
                cd = coefcd(4,9)*x + coefcd(4,8)
            else if (x .lt. borncd(6)) then
                cd = coefcd(5,8) + coefcd(5,7)/x
            else
                cd = coefcd(6,8) * (x ** 0.66146d0)
            endif
!
! --- COEFFICIENT VISCACHE 2 CFD 90 %
!
        else if (ires .eq. 1101) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,8) + coefcd(1,9)*x
            else if (x .lt. borncd(3)) then
                cd = coefcd(2,8) + coefcd(2,7)/x + coefcd(2,6)/(x*x) + coefcd(2,5)/(x*x*x)
            else
                cd = coefcd(3,8) + coefcd(3,7)/x + coefcd(3,6)/(x*x) + coefcd(3,5)/(x*x*x)
            endif
!
!
! --- COEFFICIENT VISCACHE 2 CFD 85 %
!
        else if (ires .eq. 1102) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,8) + coefcd(1,9)*x
            else
                cd = coefcd(2,8) + coefcd(2,7)/x + coefcd(2,6)/(x*x) + coefcd(2,5)/(x*x*x) + coef&
                     &cd(2,4)/(x*x*x*x)
            endif
!
!
! --- COEFFICIENT VISCACHE 2 CFD 80 %
!
        else if (ires .eq. 1103) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,8) + coefcd(1,9)*x
            else
                cd = coefcd(2,8) + coefcd(2,7)/x + coefcd(2,6)/(x*x) + coefcd(2,5)/(x*x*x)
            endif
!
!
! --- COEFFICIENT VISCACHE 2 CFD 50 %
!
        else if (ires .eq. 1104) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,8) + coefcd(1,9)*x
            else
                cd = coefcd(2,8) + coefcd(2,7)/x + coefcd(2,6)/(x*x) + coefcd(2,5)/(x*x*x)
            endif
!
!
! --- COEFFICIENT VISCACHE 2 CFD 20 %
!
        else if (ires .eq. 1105) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,8) + coefcd(1,9)*x
            else if (x .lt. borncd(3)) then
                cd = coefcd(2,8) + coefcd(2,7)/x + coefcd(2,6)/(x*x) + coefcd(2,5)/(x*x*x)
            else if (x .lt. borncd(4)) then
                cd = coefcd(3,8) + coefcd(3,7)/x + coefcd(3,6)/(x*x)
            else
                cd = coefcd(4,8) + coefcd(4,7)/x + coefcd(4,6)/(x*x) + coefcd(4,5)/(x*x*x)
            endif
!
!
! --- COEFFICIENT VISCACHE 2 CFD 10 %
!
        else if (ires .eq. 1106) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,8) + coefcd(1,9)*x
            else if (x .lt. borncd(3)) then
                cd = coefcd(2,8) + coefcd(2,7)/x + coefcd(2,6)/(x*x)
            else if (x .lt. borncd(4)) then
                cd = coefcd(3,8) + coefcd(3,7)/x + coefcd(3,6)/(x*x)
            else
                cd = coefcd(4,8) + coefcd(4,7)/x + coefcd(4,6)/(x*x) + coefcd(4,5)/(x*x*x)
            endif
!
!
! --- COEFFICIENT VISCACHE 2 TUM 90 %
!
        else if (ires .eq. 1201) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,8) + coefcd(1,9)*x
            else
                cd = coefcd(2,8) + coefcd(2,7)/x + coefcd(2,6)/(x*x) + coefcd(2,5)/(x*x*x) + coef&
                     &cd(2,4)/(x*x*x*x) + coefcd( 2,3)/(x*x*x*x*x)
            endif
!
!
! --- COEFFICIENT VISCACHE 2 TUM 86 %
!
        else if (ires .eq. 1202) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,8) + coefcd(1,9)*x
            else
                cd = coefcd(2,8) + coefcd(2,7)/x + coefcd(2,6)/(x*x) + coefcd(2,5)/(x*x*x) + coef&
                     &cd(2,4)/(x*x*x*x)
            endif
!
!
! --- COEFFICIENT VISCACHE 2 TUM 82 %
!
        else if (ires .eq. 1203) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,8) + coefcd(1,9)*x
            else
                cd = coefcd(2,8) + coefcd(2,7)/x + coefcd(2,6)/(x*x)
            endif
!
!
! --- COEFFICIENT VISCACHE 2 TUM 50 %
!
        else if (ires .eq. 1204) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,8) + coefcd(1,9)*x
            else
                cd = coefcd(2,8) + coefcd(2,7)/x + coefcd(2,6)/(x*x)
            endif
!
!
! --- COEFFICIENT VISCACHE 2 TUM 20 %
!
        else if (ires .eq. 1205) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,8) + coefcd(1,9)*x
            else
                cd = coefcd(2,8) + coefcd(2,7)/x + coefcd(2,6)/(x*x) + coefcd(2,5)/(x*x*x)
            endif
!
!
! --- COEFFICIENT VISCACHE 2 TUM 10 %
!
        else if (ires .eq. 1206) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,8) + coefcd(1,9)*x
            else
                cd = coefcd(2,8) + coefcd(2,7)/x + coefcd(2,6)/(x*x) + coefcd(2,5)/(x*x*x) + coef&
                     &cd(2,4)/(x*x*x*x)
            endif
!
        else
!
            call utmess('F', 'MODELISA4_30')
!
        endif
!
!
! **********************************************************************
! ***               FAISCEAU EN PAS TRIANGULAIRE LIGNE               ***
! **********************************************************************
!
    else if (ipas .eq. 2) then
!
        if (ires .ge. 1 .and. ires .le. 1000) then
!
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else
                if (x .lt. borncd(nborcd)) then
                    do 150 i = 2, nborcd
                        if (x .ge. borncd(i-1) .and. x .lt. borncd(i)) then
                            cd = coefcd(i-1,1)/(x*x*x*x*x*x*x) + coefcd(i-1,2)/(x*x*x*x*x*x) + co&
                                 &efcd(i-1, 3)/(x*x*x*x*x) + coefcd(i-1,4)/(x*x*x*x) + coefcd(i-1&
                                 &,5)/(x*x*x) + coefcd(i-1,6)/(x* x) + coefcd(i-1,7)/(x) + coefcd&
                                 &(i-1,8) + coefcd(i-1,9)*(x) + coefcd(i-1,10)*(x*x) + coefcd(i-1&
                                 &,11)*(x*x*x)
                            goto 160
                        endif
150                  continue
160                  continue
                else
                    cd = coefcd(nborcd,1)/(x*x*x*x*x*x*x) + coefcd( nborcd,2)/(x*x*x*x*x*x) + coe&
                         &fcd(nborcd,3)/(x*x*x* x*x) + coefcd(nborcd,4)/(x*x*x*x) + coefcd(nborcd&
                         &, 5)/(x*x*x) + coefcd(nborcd,6)/(x*x) + coefcd( nborcd,7)/(x) + coefcd(&
                         &nborcd,8) + coefcd(nborcd, 9)*(x) + coefcd(nborcd,10)*(x*x) + coefcd(nb&
                         &orcd, 11)*(x*x*x)
                endif
            endif
!
! --- CELLULE DE TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE1.
!
        else if (ires .eq. 1001) then
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,8) + coefcd(1,9)*x
            else if (x .lt. borncd(3)) then
                cd = coefcd(2,8) + coefcd(2,9)*x
            else if (x .lt. borncd(4)) then
                cd = coefcd(3,8) + coefcd(3,7)/x + coefcd(3,6)/(x*x) + coefcd(3,5)/(x*x*x)
            else if (x .lt. borncd(5)) then
                cd = coefcd(4,8) + coefcd(4,7)/x + coefcd(4,6)/(x*x)
            else
                cd = coefcd(5,9)*x
            endif
!
! --- CELLULE DE TUBES VIBRANTS EN MILIEU DE FAISCEAU VISCACHE1.
!
        else if (ires .eq. 1002) then
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,8) + coefcd(1,9)*x
            else if (x .lt. borncd(3)) then
                cd = coefcd(2,8) + coefcd(2,9)*x
            else if (x .lt. borncd(4)) then
                cd = coefcd(3,8) + coefcd(3,7)/x + coefcd(3,6)/(x*x)
            else
                cd = coefcd(4,8) + coefcd(4,9)*x
            endif
!
! --- TUBE UNIQUE VIBRANT EN MILIEU DE FAISCEAU RIGIDE VISCACHE1.
!
        else if (ires .eq. 1003) then
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,8) + coefcd(1,9)*x
            else if (x .lt. borncd(3)) then
                cd = coefcd(2,8) + coefcd(2,9)*x + coefcd(2,10)*x*x + coefcd(2,11)*x*x*x
            else if (x .lt. borncd(4)) then
                cd = coefcd(3,8) + coefcd(3,7)/x
            else
                cd = coefcd(4,8) + (0.0214d0*(x ** 1.1146d0))
            endif
!
! --- TUBE UNIQUE VIBRANT EN DEBUT DE FAISCEAU RIGIDE VISCACHE1.
!
        else if (ires .eq. 1004) then
            if (x .lt. borncd(1)) then
                cd = 0.d0
            else if (x .lt. borncd(2)) then
                cd = coefcd(1,8) + coefcd(1,9)*x
            else if (x .lt. borncd(3)) then
                cd = coefcd(2,8) + coefcd(2,9)*x + coefcd(2,10)*x*x
            else if (x .lt. borncd(4)) then
                cd = coefcd(3,8) + coefcd(3,9)*x
            else if (x .lt. borncd(5)) then
                cd = coefcd(4,8) + coefcd(4,7)/x + coefcd(4,6)/(x*x) + coefcd(4,5)/(x*x*x)
                cd = 0.95d0*cd
            else if (x .lt. borncd(6)) then
                cd = coefcd(5,8) + coefcd(5,9)*x
            else if (x .lt. borncd(7)) then
                cd = coefcd(6,8) + coefcd(6,7)/x + coefcd(6,6)/(x*x) + coefcd(6,5)/(x*x*x) + coef&
                     &cd(6,4)/(x*x*x*x)
            else if (x .lt. borncd(8)) then
                cd = coefcd(7,8) + coefcd(7,9)*x
            else
                cd = coefcd(8,8) + (0.00375d0*(x**2.76251d0))
            endif
!
        endif
!
    endif
!
! --- INVERSION DE CD POUR CONVENTION DE SIGNE ET FIN DE COEFA.
!
    cd = -cd
!
1000  continue
    call jedema()
end subroutine
