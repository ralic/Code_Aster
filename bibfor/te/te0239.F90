subroutine te0239(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES OPTIONS NON LINEAIRES
!                          COQUE 1D
!                          OPTION : 'RIGI_MECA_TANG ',
!                                   'FULL_MECA      ','RAPH_MECA      '
!                          ELEMENT: MECXSE3,METCSE3,METDSE3
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8nnem.h"
#include "asterfort/comcq1.h"
#include "asterfort/defgen.h"
#include "asterfort/dfdm1d.h"
#include "asterfort/effi.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/matdtd.h"
#include "asterfort/mattge.h"
#include "asterfort/moytpg.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "blas/dcopy.h"
    integer :: icompo, nbcou, npge, icontm, ideplm, ivectu, icou, inte, icontp
    integer :: kpki, k1, k2, kompt, ivarim, ivarip, iinstm, iinstp, lgpg, ideplp
    integer :: icarcr, nbvari, jcret, codret
    real(kind=8) :: cisail, zic, coef, rhos, rhot, epsx3, gsx3, sgmsx3
    real(kind=8) :: zmin, hic, depsx3
    integer :: nbres, itab(8), jnbspi
    parameter (nbres=2)
    character(len=16) :: nomres(nbres), option, nomte
    character(len=8) ::  nompar, elrefe
    integer :: valret(nbres)
    real(kind=8) :: valres(nbres), tempm, tempp
    real(kind=8) :: dfdx(3), zero, un, deux
    real(kind=8) :: test, test2, eps, nu, h, cosa, sina, cour, r
    real(kind=8) :: jacp, kappa, correc
    real(kind=8) :: eps2d(4), deps2d(4), sigtdi(5), sigmtd(5)
    real(kind=8) :: x3
    real(kind=8) :: dtild(5, 5), dtildi(5, 5), dsidep(6, 6)
    real(kind=8) :: rtangi(9, 9), rtange(9, 9), sigm2d(4), sigp2d(4)
    real(kind=8) :: angmas(3)
    integer :: nno, nnos, jgano, ndim, kp, npg, i, j, k, imatuu, icaco, ndimv
    integer :: ivarix, mod
    integer :: ipoids, ivf, idfdk, igeom, imate
    integer :: nbpar, cod, iret, ksp
    aster_logical :: vecteu, matric, testl1, testl2
!
    parameter (npge=3)
!
    data zero,un,deux/0.d0,1.d0,2.d0/
!
    ivarip=1
!
    eps = 1.d-3
    codret = 0
!
!     ANGLE DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
!     INITIALISE A R8NNEM (ON NE S'EN SERT PAS)
    angmas(1) = r8nnem()
    angmas(2) = r8nnem()
    angmas(3) = r8nnem()
!
!
    vecteu = ((option.eq.'FULL_MECA') .or. (option.eq.'RAPH_MECA'))
    matric = ((option.eq.'FULL_MECA') .or. (option.eq.'RIGI_MECA_TANG'))
!
    call elref1(elrefe)
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfdk, jgano=jgano)
!
!       TYPMOD(1) = 'C_PLAN  '
!       TYPMOD(2) = '        '
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCACOQU', 'L', icaco)
    h = zr(icaco)
    kappa = zr(icaco+1)
    correc = zr(icaco+2)
!
!---- COTE MINIMALE SUR L'EPAISSEUR
    zmin = -h/2.d0
!
    call jevech('PMATERC', 'L', imate)
    nomres(1) = 'E'
    nomres(2) = 'NU'
!
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PCONTMR', 'L', icontm)
    call jevech('PINSTPR', 'L', iinstp)
    call jevech('PDEPLPR', 'L', ideplp)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PCARCRI', 'L', icarcr)
!
    if (zk16(icompo+3) .eq. 'COMP_ELAS') then
        if (zk16(icompo) .ne. 'ELAS') then
            call utmess('F', 'ELEMENTS2_90')
        endif
    endif
!
    if (zk16(icompo+2) (6:10) .eq. '_REAC') then
        call utmess('A', 'ELEMENTS3_54')
    endif
!
!--- LECTURE DU NBRE DE VAR. INTERNES, DE COUCHES ET LONG. MAX DU
!--- POINT D'INTEGRATION
    read (zk16(icompo-1+2),'(I16)') nbvari
    call tecach('OON', 'PVARIMR', 'L', iret, nval=7,&
                itab=itab)
!      LGPG = MAX(ITAB(6),1)*ITAB(7) resultats faux sur Bull avec ifort
    if (itab(6) .le. 1) then
        lgpg=itab(7)
    else
        lgpg = itab(6)*itab(7)
    endif
    call jevech('PNBSP_I', 'L', jnbspi)
    nbcou = zi(jnbspi-1+1)
!---- MESSAGES LIMITATION NBRE DE COUCHES
    if (nbcou .le. 0) then
        call utmess('F', 'ELEMENTS_12')
    endif
    if (nbcou .gt. 10) then
        call utmess('F', 'ELEMENTS3_55')
    endif
!---- EPAISSEUR DE CHAQUE COUCHE
    hic = h/nbcou
    if (vecteu) then
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
    endif
    if (vecteu) then
        ndimv = npg*npge*nbcou*nbvari
        call jevech('PVARIMP', 'L', ivarix)
        call dcopy(ndimv, zr(ivarix), 1, zr(ivarip), 1)
    endif
    if (matric) then
        call jevech('PMATUUR', 'E', imatuu)
!        IVARIP=IVARIM
    endif
!
    call r8inir(81, 0.d0, rtange, 1)
    kpki = 0
!-- DEBUT DE BOUCLE D'INTEGRATION SUR LA SURFACE NEUTRE
    do 140 kp = 1, npg
        k = (kp-1)*nno
        call dfdm1d(nno, zr(ipoids+kp-1), zr(idfdk+k), zr(igeom), dfdx,&
                    cour, jacp, cosa, sina)
        r = zero
!
        call r8inir(5, 0.d0, sigmtd, 1)
        call r8inir(25, 0.d0, dtild, 1)
!
!-- BOUCLE SUR LES POINTS D'INTEGRATION SUR LA SURFACE
!
        do 30 i = 1, nno
            r = r + zr(igeom+2*i-2)*zr(ivf+k+i-1)
 30     continue
!
!===============================================================
!     -- RECUPERATION DE LA TEMPERATURE POUR LE MATERIAU:
!     -- SI LA TEMPERATURE EST CONNUE AUX NOEUDS :
        call moytpg('RIGI', kp, 3, '-', tempm,&
                    iret)
        nbpar = 1
        nompar = 'TEMP'
        call rcvalb('RIGI', kp, 1, '-', zi(imate),&
                    ' ', 'ELAS', nbpar, nompar, [tempm],&
                    2, nomres, valres, valret, 1)
!
        nu = valres(2)
        cisail = valres(1)/ (un+nu)
!
        if (nomte .eq. 'MECXSE3') jacp = jacp*r
!
        test = abs(h*cour/deux)
        if (test .ge. un) correc = zero
        test2 = abs(h*cosa/ (deux*r))
        if (test2 .ge. un) correc = zero
!
        testl1 = (test.le.eps .or. correc.eq.zero)
        testl2 = (&
                 test2 .le. eps .or. correc .eq. zero .or. abs(cosa) .le. eps .or. abs(cour*r)&
                 .le. eps .or. abs(cosa-cour*r) .le. eps&
                 )
!
!-- DEBUT DE BOUCLE D'INTEGRATION DANS L'EPAISSEUR
!
        do 110 icou = 1, nbcou
            do 100 inte = 1, npge
                if (inte .eq. 1) then
                    zic = zmin + (icou-1)*hic
                    coef = 1.d0/3.d0
                else if (inte.eq.2) then
                    zic = zmin + hic/2.d0 + (icou-1)*hic
                    coef = 4.d0/3.d0
                else
                    zic = zmin + hic + (icou-1)*hic
                    coef = 1.d0/3.d0
                endif
!
                x3 = zic
!
                if (testl1) then
                    rhos = 1.d0
                else
                    rhos = 1.d0 + x3*cour
                endif
                if (testl2) then
                    rhot = 1.d0
                else
                    rhot = 1.d0 + x3*cosa/r
                endif
!
!           CALCULS DES COMPOSANTES DE DEFORMATIONS TRIDIMENSIONNELLES :
!           EPSSS, EPSTT, EPSSX3 (EN FONCTION DES DEFORMATIONS
!           GENERALISEES :ESS,KSS,ETT,KTT,GS)
!           DE L'INSTANT PRECEDANT ET DES DEFORMATIONS INCREMENTALES
!           DE L'INSTANT PRESENT
!
                call defgen(testl1, testl2, nno, r, x3,&
                            sina, cosa, cour, zr( ivf+k), dfdx,&
                            zr(ideplm), eps2d, epsx3)
                call defgen(testl1, testl2, nno, r, x3,&
                            sina, cosa, cour, zr( ivf+k), dfdx,&
                            zr(ideplp), deps2d, depsx3)
!
!           COQUE_D_PLAN
                if (nomte .eq. 'METDSE3') then
                    eps2d(2) = 0.d0
                    deps2d(2) = 0.d0
                    mod=2
!           COQUE_C_PLAN
                else if (nomte.eq.'METCSE3') then
                    eps2d(2) = 0.d0
                    deps2d(2) = 0.d0
                    mod=-1
!           COQUE_AXIS
                else if (nomte.eq.'MECXSE3') then
                    mod=1
                endif
!
!           CONSTRUCTION DE LA DEFORMATION GSX3
!           ET DE LA CONTRAINTE SGMSX3
                gsx3 = 2.d0* (epsx3+depsx3)
                sgmsx3 = cisail*kappa*gsx3/2.d0
!
!           CALCUL DU NUMERO DU POINT D'INTEGRATION COURANT
                kpki = kpki + 1
                k1 = 4* (kpki-1)
                k2 = lgpg* (kp-1) + (npge* (icou-1)+inte-1)*nbvari
                ksp=(icou-1)*npge + inte
!
                do 55 i = 1, 4
                    sigm2d(i)=zr(icontm+k1+i-1)
 55             continue
!
!  APPEL AU COMPORTEMENT
                call comcq1('RIGI', kp, ksp, mod, zi(imate),&
                            zk16(icompo), zr(icarcr), zr(iinstm), zr(iinstp), eps2d,&
                            deps2d, tempm, tempp, sigm2d, zr(ivarim+k2),&
                            option, angmas, sigp2d, zr( ivarip+k2), dsidep,&
                            cod)
                if (vecteu) then
                    do 56 i = 1, 4
                        zr(icontp+k1+i-1)=sigp2d(i)
 56                 continue
                endif
!
!           COD=1 : ECHEC INTEGRATION LOI DE COMPORTEMENT
!           COD=3 : C_PLAN DEBORST SIGZZ NON NUL
                if (cod .ne. 0) then
                    if (codret .ne. 1) then
                        codret = cod
                    endif
                endif
!
!
                if (matric) then
!-- CALCULS DE LA MATRICE TANGENTE : BOUCLE SUR L'EPAISSEUR
!-- CONSTRUCTION DE LA MATRICE DTD (DTILD)
                    call matdtd(nomte, testl1, testl2, dsidep, cisail,&
                                x3, cour, r, cosa, kappa,&
                                dtildi)
                    do 80 i = 1, 5
                        do 70 j = 1, 5
                            dtild(i,j) = dtild(i,j) + dtildi(i,j)* 0.5d0*hic*coef
 70                     continue
 80                 continue
                endif
!
                if (vecteu) then
!-- CALCULS DES EFFORTS INTERIEURS : BOUCLE SUR L'EPAISSEUR
                    if (nomte .eq. 'MECXSE3') then
                        sigtdi(1) = zr(icontp-1+k1+1)/rhos
                        sigtdi(2) = x3*zr(icontp-1+k1+1)/rhos
                        sigtdi(3) = zr(icontp-1+k1+2)/rhot
                        sigtdi(4) = x3*zr(icontp-1+k1+2)/rhot
                        sigtdi(5) = sgmsx3/rhos
                    else
                        sigtdi(1) = zr(icontp-1+k1+1)/rhos
                        sigtdi(2) = x3*zr(icontp-1+k1+1)/rhos
                        sigtdi(3) = sgmsx3/rhos
                        sigtdi(4) = 0.d0
                        sigtdi(5) = 0.d0
                    endif
!
                    do 90 i = 1, 5
                        sigmtd(i) = sigmtd(i) + sigtdi(i)*0.5d0*hic* coef
 90                 continue
                endif
!-- FIN DE BOUCLE SUR LES POINTS D'INTEGRATION DANS L'EPAISSEUR
100         continue
110     continue
!
        if (vecteu) then
!-- CALCUL DES EFFORTS INTERIEURS
            call effi(nomte, sigmtd, zr(ivf+k), dfdx, jacp,&
                      sina, cosa, r, zr(ivectu))
        endif
        if (matric) then
!-- CONSTRUCTION DE LA MATRICE TANGENTE
            call mattge(nomte, dtild, sina, cosa, r,&
                        jacp, zr(ivf+k), dfdx, rtangi)
            do 130 i = 1, 9
                do 120 j = 1, 9
                    rtange(i,j) = rtange(i,j) + rtangi(i,j)
120             continue
130         continue
        endif
!-- FIN DE BOUCLE SUR LES POINTS D'INTEGRATION DE LA SURFACE NEUTRE
140 end do
    if (matric) then
!-- STOCKAGE DE LA MATRICE TANGENTE
        kompt = 0
        do 160 j = 1, 9
            do 150 i = 1, j
                kompt = kompt + 1
                zr(imatuu-1+kompt) = rtange(i,j)
150         continue
160     continue
    endif
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = codret
    endif
end subroutine
