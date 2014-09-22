subroutine te0237(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm1d.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
    character(len=16) :: option, nomte
! ......................................................................
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
! .  - FONCTION REALISEE:      CONTRAINTES PLANES AUX NOEUDS
! .                            COQUE 1D
! .                        OPTION  : 'SIEF_ELGA'
! .                                  'EPSI_ELGA'
! .                        ELEMENT: MECXSE3,METCSE3,METDSE3
! .  - ARGUMENTS:
! .      DONNEES:      OPTION       -->  OPTION DE CALCUL
! .                    NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    character(len=8) :: elrefe, nompar
    character(len=16) :: nomres(3)
    integer :: icodre(3)
    real(kind=8) :: e, nu, tpg, tpgmoy, tpginf, tpgsup, valpar, tref
    real(kind=8) :: x3, eps(5), c1, c2, h, epsthe, ki(3), niv
    real(kind=8) :: e11, e22, k11, k22, ep11, ep22, ep12, esx3
    real(kind=8) :: dfdx(3), valres(3)
    real(kind=8) :: jac, r, cosa, sina, cour, correc, zmin, hic
    integer :: i, k, kp, igeom, imate, icaco, idepl, icont, nbpar, idefor
    integer :: itab(7)
    integer :: nno, npg, idfdk, ivf, iret, iret1, iret2, iret3, idec, inte, npge
!
!-----------------------------------------------------------------------
    integer :: icou, ipoids, iret4, isp, jgano, jnbspi, nbcmp
    integer :: nbcou, ndim, nnos
    real(kind=8) :: si11, si12, si22, zic
!-----------------------------------------------------------------------
    call elref1(elrefe)
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfdk,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCACOQU', 'L', icaco)
    call jevech('PDEPLAR', 'L', idepl)
    call jevech('PNBSP_I', 'L', jnbspi)
    nbcou=zi(jnbspi-1+1)
    if (nbcou .le. 0) then
        call utmess('F', 'ELEMENTS_12')
    endif
    if (nbcou .gt. 30) then
        call utmess('F', 'ELEMENTS3_50')
    endif
!
    if (option .eq. 'EPSI_ELGA') then
        call tecach('OOO', 'PDEFOPG', 'E', iret, nval=7,&
                    itab=itab)
        idefor=itab(1)
    else if (option.eq.'SIEF_ELGA') then
        call jevech('PMATERC', 'L', imate)
        call tecach('OOO', 'PCONTRR', 'E', iret, nval=7,&
                    itab=itab)
        icont=itab(1)
        call rcvarc(' ', 'TEMP', 'REF', 'RIGI', 1,&
                    1, tref, iret)
    else
        ASSERT(.false.)
    endif
!
    nbcmp=itab(2)/itab(3)
    ASSERT(nbcmp.gt.0)
!
    h=zr(icaco)
!---- COTE MINIMALE SUR L'EPAISSEUR
    zmin=-h/2.d0
!---- EPAISSEUR DE CHAQUE COUCHE
    hic=h/nbcou
    correc=zr(icaco+2)
! NOMBRE DE POINT DE GAUSS DANS LA TRANCHE
! (POUR RESTER COHERENT AVEC SIEF_ELGA EN PLASTICITE )
    npge=3
    ki(1)=-1.d0
    ki(2)=0.d0
    ki(3)=1.d0
!
    do 50 icou = 1, nbcou
        do 40 inte = 1, npge
            niv=ki(inte)
!
            if (inte .eq. 1) then
                zic=zmin+(icou-1)*hic
            else if (inte.eq.2) then
                zic=zmin+hic/2.d0+(icou-1)*hic
            else
                zic=zmin+hic+(icou-1)*hic
            endif
            x3=zic
!
            do 30 kp = 1, npg
                k=(kp-1)*nno
                idec=nbcmp*(kp-1)*npge*nbcou+ nbcmp*(icou-1)*npge+&
                nbcmp*(inte-1)
                call dfdm1d(nno, zr(ipoids+kp-1), zr(idfdk+k), zr(igeom), dfdx,&
                            cour, jac, cosa, sina)
!
                do 10 i = 1, 5
                    eps(i)=0.d0
10              continue
                r=0.d0
                do 20 i = 1, nno
                    eps(1)=eps(1)+dfdx(i)*zr(idepl+3*i-3)
                    eps(2)=eps(2)+dfdx(i)*zr(idepl+3*i-2)
                    eps(3)=eps(3)+dfdx(i)*zr(idepl+3*i-1)
                    eps(4)=eps(4)+zr(ivf+k+i-1)*zr(idepl+3*i-3)
                    eps(5)=eps(5)+zr(ivf+k+i-1)*zr(idepl+3*i-1)
                    r=r+zr(ivf+k+i-1)*zr(igeom+2*i-2)
20              continue
!
                e11=eps(2)*cosa-eps(1)*sina
                k11=eps(3)
                esx3=eps(5)+eps(1)*cosa+eps(2)*sina
                if (nomte .eq. 'MECXSE3') then
                    e22=eps(4)/r
                    k22=-eps(5)*sina/r
                    ep22=(e22+x3*k22)/(1.d0+(correc*x3*cosa/r))
                else
                    e22=0.d0
                    k22=0.d0
                    ep22=0.d0
                endif
!
                ep11=(e11+x3*k11)/(1.d0+(correc*x3*cour))
                ep12=esx3/(1.d0+(correc*x3*cour))
!
                if (option .eq. 'EPSI_ELGA') then
                    zr(idefor+idec-1+1)=ep11
                    zr(idefor+idec-1+2)=ep22
                    zr(idefor+idec-1+3)=ep12
!
                else if (option.eq.'SIEF_ELGA') then
!
!         -- RECUPERATION DES PARAMETRES MATERIAU :
!
!         ---- RECUPERATION DE LA TEMPERATURE POUR LE MATERIAU:
!         ---- SI LA TEMPERATURE EST CONNUE AUX POINTS DE GAUSS :
                    isp=3*(icou-1)
                    call rcvarc(' ', 'TEMP', '+', 'RIGI', kp,&
                                isp+1, tpginf, iret1)
                    call rcvarc(' ', 'TEMP', '+', 'RIGI', kp,&
                                isp+2, tpgmoy, iret2)
                    call rcvarc(' ', 'TEMP', '+', 'RIGI', kp,&
                                isp+3, tpgsup, iret3)
                    iret4=iret1+iret2+iret3
                    ASSERT(iret4.eq.0 .or. iret4.eq.3)
!
!         ---- UTILISATION DE 4 POINTS DE GAUSS DANS L'EPAISSEUR
!         ---- COMME POUR LA LONGUEUR
!
                    if (iret4 .eq. 0) then
                        tpg=tpgsup*niv*(1.d0+niv)/2.d0+tpgmoy*(1.d0-(&
                        niv)**2)- tpginf*niv*(1.d0-niv)/2.d0
                    else
                        tpg=r8nnem()
                    endif
                    valpar=tpg
                    nbpar=1
                    nompar='TEMP'
!
                    nomres(1)='E'
                    nomres(2)='NU'
                    nomres(3)='ALPHA'
                    call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                                ' ', 'ELAS', nbpar, nompar, [valpar],&
                                2, nomres, valres, icodre, 1)
                    call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                                ' ', 'ELAS', nbpar, nompar, [valpar],&
                                1, nomres(3), valres(3), icodre( 3), 0)
                    e=valres(1)
                    nu=valres(2)
                    if (iret4 .eq. 0) then
                        if ((icodre(3).ne.0) .or. (iret.eq.1)) then
                            call utmess('F', 'CALCULEL_15')
                        else
                            epsthe=(tpg-tref)*valres(3)*e/(1.d0-nu)
                        endif
                    else
                        epsthe=0.d0
                    endif
!
!         -- FIN RECUPERATION DES PARAMETRES MATERIAU :
!
                    c1=e/(1.d0+nu)
                    c2=c1/(1.d0-nu)
!
                    if (nomte.eq.'MECXSE3') then
                        si11=c2*(ep11+nu*ep22)-epsthe
                        si22=c2*(ep22+nu*ep11)-epsthe
                    else if (nomte.eq.'METDSE3 ') then
                        si11=c2*ep11-epsthe
                        si22=c2*nu*ep11-epsthe
                    else
                        si11=e*(ep11-epsthe)
                        si22=0.d0
                    endif
                    si12=c1*ep12
!
                    zr(icont+idec-1+1)=si11
                    zr(icont+idec-1+2)=si22
                    zr(icont+idec-1+4)=si12
!
                else
                    ASSERT(.false.)
                endif
!
30          continue
!
40      continue
50  end do
!
end subroutine
