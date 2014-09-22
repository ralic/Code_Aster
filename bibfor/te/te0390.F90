subroutine te0390(option, nomte)
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
    implicit none
!
#include "jeveux.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/gddyng.h"
#include "asterfort/gdecva.h"
#include "asterfort/gdfine.h"
#include "asterfort/gdfint.h"
#include "asterfort/gdjrg0.h"
#include "asterfort/gdliva.h"
#include "asterfort/gdmine.h"
#include "asterfort/gdmrig.h"
#include "asterfort/gdpetk.h"
#include "asterfort/gdsig.h"
#include "asterfort/gdstag.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - ELEMENT:  MECA_POU_D_T_GD
!      OPTION : 'FULL_MECA'   'RAPH_MECA'   'RIGI_MECA_TANG'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    real(kind=8) :: nu, instam, instap
    character(len=8) :: elrefe
    character(len=16) :: nomres(4)
    integer :: icodre(4)
    real(kind=8) :: en(3, 2), enprim(3, 2), valres(4), granc(6), grani(4)
    real(kind=8) :: rigi(18, 18), fint(6, 3), y0(3), x00(3, 3), x0k(3, 3)
    real(kind=8) :: x0pg(3), tetak(3, 3), tetag(3), tetapg(3), qim(3, 3)
    real(kind=8) :: qikm1(3, 3), qik(3, 3), rot0(3, 3), rotm(3, 3), rotkm1(3, 3)
    real(kind=8) :: rotk(3, 3), petik(3), petikm(3), gn(3), gm(3), pn(3), pm(3)
    real(kind=8) :: x0sk(3, 3), rmkm1(3, 3), rmk(3, 3), omkm1(3, 3)
    real(kind=8) :: ompkm1(3, 3), omk(3, 3), ompk(3, 3), x0sec(3), rgmkm(3)
    real(kind=8) :: rgmk(3), omgkm(3), ompgkm(3), omgk(3), ompgk(3)
!
!
!-----------------------------------------------------------------------
    integer :: i, iacckm, iaccp, ico, icompo, iddepl, idepde
    integer :: idepkm, idepm, idfdk, ifint, igeom, imat, imate
    integer :: imatuu, instmr, instpr, ipoids, iret, iromk, iromkm
    integer :: istady, ivarim, ivarip, ivf, ivitkm, ivitp, j
    integer :: jcret, jefint, jgano, k0, k1, k2, k3
    integer :: k4, k5, k6, k7, kc, kp, ks
    integer :: lorien, lsect, lsig, lsigma, ndim, ne, nno
    integer :: nnos, nord, npg
    real(kind=8) :: a, ajacob, alfnmk, ay, az, delnmk, demi
    real(kind=8) :: deux, e, g, pas, pjacob, r8bid=0.d0, rho
    real(kind=8) :: stoudy, un, xiy, xiz, xjx, zero
!-----------------------------------------------------------------------
    call elref1(elrefe)
    if (option .eq. 'FORC_NODA') goto 210
!
    zero = 0.d0
    demi = 5.d-1
    un = 1.d0
    deux = 2.d0
!
!* STOUDY VAUT: 1., SI L'ON EST EN DYNAMIQUE
!*              0., SI L'ON EST EN STATIQUE
!
    call tecach('NNN', 'PSTADYN', 'L', iret, iad=istady)
    if (istady .eq. 0) then
        stoudy = 0.d0
    else
        stoudy = zr(istady)
    endif
!
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfdk,jgano=jgano)
!
    ico = 0
    do 20 kp = 1, npg
        do 10 ne = 1, nno
            ico = ico + 1
            en(ne,kp) = zr(ivf-1+ico)
            enprim(ne,kp) = zr(idfdk-1+ico)
10      continue
20  end do
!
!
! PARAMETRES EN ENTREE
    call jevech('PCOMPOR', 'L', icompo)
    if (zk16(icompo) (1:4) .ne. 'ELAS') then
        call utmess('F', 'ELEMENTS3_85', sk=zk16(icompo))
    endif
    if (zk16(icompo+2) .ne. 'GROT_GDEP') then
        call utmess('F', 'ELEMENTS3_86', sk=zk16(icompo+2))
    endif
    call jevech('PMATERC', 'L', imate)
    nomres(1) = 'E'
    nomres(2) = 'NU'
    nomres(3) = 'RHO'
    nomres(4) = 'ALPHA'
    r8bid=0.d0
    call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                ' ', 'ELAS', 0, '  ', [r8bid],&
                2, nomres, valres, icodre, 1)
    call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                ' ', 'ELAS', 0, '  ', [r8bid],&
                1, nomres(3), valres(3), icodre(3), 0)
    if (icodre(3) .ne. 0) then
        if (stoudy .gt. demi) then
            call utmess('F', 'ELEMENTS3_87')
        else
            valres(3) = zero
        endif
    endif
    e = valres(1)
    nu = valres(2)
    rho = valres(3)
    g = e/ (deux* (un+nu))
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
    call jevech('PCAGNPO', 'L', lsect)
    lsect = lsect - 1
!
!     --- LA SECTION EST SUPPOSEE CONSTANTE ---
    a = zr(lsect+1)
    xiy = zr(lsect+2)
    xiz = zr(lsect+3)
    ay = zr(lsect+4)
    az = zr(lsect+5)
    xjx = zr(lsect+8)
    granc(1) = e*a
!     GRANC(1) = 1.D6
    granc(2) = g*a/ay
    granc(3) = g*a/az
    granc(4) = g*xjx
    granc(5) = e*xiy
    granc(6) = e*xiz
!
!     --- RECUPERATION DES ORIENTATIONS INITIALES Y0(1), Y0(2), Y0(3)
    call jevech('PCAORIE', 'L', lorien)
    y0(1) = zr(lorien)
    y0(2) = zr(lorien+1)
    y0(3) = zr(lorien+2)
! PARAMETRES EN SORTIE
    if (option(1:9) .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
        call jevech('PMATUNS', 'E', imatuu)
!
        nord = 6*nno
        do 40 j = 1, nord
            do 30 i = 1, nord
                rigi(i,j) = zero
30          continue
40      continue
    endif
    if (option(1:9) .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
        call jevech('PVECTUR', 'E', jefint)
        call jevech('PCONTPR', 'E', lsigma)
        do 60 ne = 1, nno
            do 50 kc = 1, 6
                fint(kc,ne) = zero
50          continue
60      continue
    endif
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PDEPLMR', 'L', idepm)
! ---- LA PRESENCE DU CHAMP DE DEPLACEMENT A L INSTANT T+
! ---- DEVRAIT ETRE CONDITIONNE  PAR L OPTION (AVEC RIGI_MECA_TANG
! ---- CA N A PAS DE SENS).
! ---- CEPENDANT CE CHAMP EST INITIALISE A 0 PAR LA ROUTINE NMMATR.
    call jevech('PDEPLPR', 'L', idepde)
    call jevech('PDDEPLA', 'L', iddepl)
!
!
    k0 = igeom - 1
    k1 = idepm - 1
    k2 = idepde - 1
    k3 = iddepl - 1
!
    do 100 ne = 1, nno
        do 80 kc = 1, 3
            k0 = k0 + 1
            k1 = k1 + 1
            k2 = k2 + 1
            k3 = k3 + 1
            x00(kc,ne) = zr(k0)
!
            if (option .eq. 'RIGI_MECA_TANG') then
                x0k(kc,ne) = zr(k0) + zr(k1)
            else
                x0k(kc,ne) = zr(k0) + zr(k1) + zr(k2)
            endif
!
80      continue
        do 90 kc = 1, 3
            k1 = k1 + 1
            k2 = k2 + 1
            k3 = k3 + 1
            qim(kc,ne) = zr(k1)
!
            if (option .eq. 'RIGI_MECA_TANG') then
                qik(kc,ne) = 0.d0
                tetak(kc,ne) = 0.d0
            else
                qik(kc,ne) = zr(k2)
                tetak(kc,ne) = zr(k3)
            endif
!
90      continue
100  end do
!
    call jevech('PVARIMP', 'L', ivarim)
    if (stoudy .gt. demi) then
!* ON TRAITE UN PROBLEME DYNAMIQUE
        call jevech('PINSTMR', 'L', instmr)
        call jevech('PINSTPR', 'L', instpr)
        instam = zr(instmr)
        instap = zr(instpr)
        pas = instap - instam
        grani(1) = rho*xjx
        grani(2) = rho*xiy
        grani(3) = rho*xiz
        grani(4) = rho*a
!* PARAMETRES ALPHA ET DELTA DE NEWMARK
        alfnmk = zr(istady+1)
        delnmk = zr(istady+2)
!
        call jevech('PDEPKM1', 'L', idepkm)
        call jevech('PVITKM1', 'L', ivitkm)
        call jevech('PACCKM1', 'L', iacckm)
        call jevech('PVITPLU', 'L', ivitp)
        call jevech('PACCPLU', 'L', iaccp)
        call jevech('PROMKM1', 'L', iromkm)
        call jevech('PROMK', 'L', iromk)
        k1 = idepkm - 1
        k2 = ivitkm - 1
        k3 = iacckm - 1
        k4 = ivitp - 1
        k5 = iaccp - 1
        k6 = iromkm - 1
        k7 = iromk - 1
        do 130 ne = 1, nno
            do 110 kc = 1, 3
                k1 = k1 + 1
                k2 = k2 + 1
                k3 = k3 + 1
                k4 = k4 + 1
                k5 = k5 + 1
                k6 = k6 + 1
                k7 = k7 + 1
                x0sk(kc,ne) = zr(k5)
110          continue
            do 120 kc = 1, 3
                k1 = k1 + 1
                k2 = k2 + 1
                k3 = k3 + 1
                k4 = k4 + 1
                k5 = k5 + 1
                k6 = k6 + 1
                k7 = k7 + 1
                qikm1(kc,ne) = zr(k1)
                omkm1(kc,ne) = zr(k2)
                ompkm1(kc,ne) = zr(k3)
                omk(kc,ne) = zr(k4)
                ompk(kc,ne) = zr(k5)
                rmkm1(kc,ne) = zr(k6)
                rmk(kc,ne) = zr(k7)
120          continue
130      continue
    endif
!
!* BOUCLE SUR LES POINTS DE GAUSS
!
    do 160 kp = 1, npg
        call gdjrg0(kp, nno, enprim, x00, y0,&
                    ajacob, rot0)
        pjacob = zr(ipoids-1+kp)*ajacob
!*** LECTURE, DANS 'PVARIMR', DU VECTEUR-COURBURE A L'ITER. PRECEDENTE
        call gdliva(kp, zr(ivarim), petikm)
!
        call gdstag(stoudy, kp, nno, ajacob, en,&
                    enprim, x0k, tetak, qim, qikm1,&
                    qik, x0pg, tetag, tetapg, rotm,&
                    rotkm1, rotk)
        call gdpetk(tetag, tetapg, petikm, petik)
!
!*** ECRITURE, DANS 'PVARIPR', DU VECTEUR-COURBURE ACTUALISE, CAR
!*** MAJSVT, UTILE POUR D'AUTRES ELEMENTS, COPIE VARIP DANS VARIM
        if (option .ne. 'RIGI_MECA_TANG') then
            call jevech('PVARIPR', 'E', ivarip)
            call gdecva(kp, petik, zr(ivarip))
        endif
!
        call gdsig('RIGI', kp, 1, x0pg, petik,&
                   rot0, rotk, granc, zi(imate), gn,&
                   gm, pn, pm)
        if (stoudy .gt. demi) then
!* ON TRAITE UN PROBLEME DYNAMIQUE
            call gddyng(kp, nno, en, x0sk, rmkm1,&
                        rmk, omkm1, ompkm1, omk, ompk,&
                        x0sec, rgmkm, rgmk, omgkm, ompgkm,&
                        omgk, ompgk)
        endif
!
        if (option(1:9) .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
            call gdmrig(kp, nno, ajacob, pjacob, en,&
                        enprim, x0pg, rot0, rotk, granc,&
                        pn, pm, rigi)
            if (stoudy .gt. demi) then
!* ON TRAITE UN PROBLEME DYNAMIQUE
                call gdmine(kp, nno, pjacob, en, grani,&
                            alfnmk, delnmk, pas, rot0, rotm,&
                            rotkm1, rotk, rmkm1, rmk, omgkm,&
                            ompgkm, omgk, ompgk, rigi)
            endif
        endif
!
        if (option(1:9) .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
            call gdfint(kp, nno, ajacob, pjacob, en,&
                        enprim, x0pg, pn, pm, fint)
            lsig = lsigma - 1 + (kp-1)*6
            do 140 ks = 1, 3
                lsig = lsig + 1
!*** ATTENTION : LE TORSEUR EST EXPRIME EN COORDONNEES LOCALES
                zr(lsig) = gn(ks)
140          continue
            do 150 ks = 1, 3
                lsig = lsig + 1
                zr(lsig) = gm(ks)
150          continue
            if (stoudy .gt. demi) then
                call gdfine(kp, nno, pjacob, en, grani,&
                            rot0, rotk, omgk, ompgk, fint)
            endif
        endif
!
!* FIN DE BOUCLE SUR LES POINTS DE GAUSS
!
160  end do
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
        imat = imatuu - 1
        do 180 i = 1, nord
            do 170 j = 1, nord
                imat = imat + 1
                zr(imat) = rigi(i,j)
170          continue
180      continue
    endif
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
        ifint = jefint - 1
        do 200 ne = 1, nno
            do 190 kc = 1, 6
                ifint = ifint + 1
                zr(ifint) = fint(kc,ne)
190          continue
200      continue
!
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = 0
    endif
210  continue
end subroutine
