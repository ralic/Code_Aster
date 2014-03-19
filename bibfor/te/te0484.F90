subroutine te0484(option, nomte)
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
    implicit none
!
!          ELEMENT SHB
!    FONCTION REALISEE:
!            OPTION : 'RIGI_MECA      '
!                            CALCUL DES MATRICES ELEMENTAIRES  3D
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!
#include "jeveux.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/idsshb.h"
#include "asterfort/jevech.h"
#include "asterfort/moytem.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/sh1for.h"
#include "asterfort/sh2for.h"
#include "asterfort/sh6for.h"
#include "asterfort/sh8for.h"
#include "asterfort/tecach.h"
!
!-----------------------------------------------------------------------
    integer :: i, icompo, icontm, ideplm, idfde, igeom, imate
    integer :: ipoids, iret, ivectu, ivf, j, jgano
    integer :: nbres, nbv, ndim, nno, nnos, npg
    real(kind=8) :: tempm
!-----------------------------------------------------------------------
    parameter (nbres=2)
    character(len=4) :: fami
    integer :: icodre(nbres), kpg, spt
    character(len=8) :: nomres(nbres), famil, poum
    character(len=16) :: nomte, nomshb, option
    real(kind=8) :: sigma(120), xidepm(60)
    real(kind=8) :: fstab(12), para(2)
    real(kind=8) :: valres(nbres)
    real(kind=8) :: nu, e
    fami = 'RIGI'
    call elrefe_info(fami=fami,ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
! --- INITIALISATIONS :
    call idsshb(ndim, nno, npg, nomshb)
    do 10 i = 1, 2
        para(i) = 0.d0
10  continue
    famil='FPG1'
    kpg=1
    spt=1
    poum='+'
    if (option .eq. 'FORC_NODA') then
! ----  RECUPERATION DES COORDONNEES DES CONNECTIVITES
        call jevech('PGEOMER', 'L', igeom)
! ----  RECUPERATION DU MATERIAU DANS ZI(IMATE)
        call jevech('PMATERC', 'L', imate)
        nomres(1) = 'E'
        nomres(2) = 'NU'
        nbv = 2
! ----  INTERPOLATION DES COEFFICIENTS EN FONCTION DE LA TEMPERATURE
! ----  ET DU TEMPS
!
        call moytem(fami, npg, 1, '+', tempm,&
                    iret)
        call rcvalb(famil, kpg, spt, poum, zi(imate),&
                    ' ', 'ELAS', 1, 'TEMP', [tempm],&
                    nbv, nomres, valres, icodre, 1)
        e = valres(1)
        nu = valres(2)
! ----  PARAMETRES MATERIAUX
        para(1) = e
        para(2) = nu
    endif
!
!  ==============================================
!  -- VECTEUR DES FORCES INTERNES
!  ==============================================
    if (option .eq. 'FORC_NODA') then
        call jevech('PGEOMER', 'L', igeom)
! ----     CONTRAINTES AUX POINTS D'INTEGRATION
        call jevech('PCONTMR', 'L', icontm)
!         CHAMPS POUR LA REACTUALISATION DE LA GEOMETRIE
        call jevech('PDEPLMR', 'L', ideplm)
! ----     CONTRAINTES DE STABILISATION
! ----     PARAMETRES EN SORTIE
! ----     VECTEUR DES FORCES INTERNES (BT*SIGMA)
        call jevech('PVECTUR', 'E', ivectu)
        call tecach('ONN', 'PCOMPOR', 'L', iret, iad=icompo)
        if (icompo .ne. 0) then
            call jevech('PCOMPOR', 'L', icompo)
        endif
!  =============================================
!  -  ACTUALISATION : GEOM ORIG + DEPL DEBUT PAS
!  =============================================
        call r8inir(60, 0.d0, xidepm, 1)
        if ((zk16(icompo+2).eq.'GROT_GDEP')) then
            call r8inir(60, 0.d0, xidepm, 1)
            do 150 i = 1, 3*nno
                zr(igeom+i-1) = zr(igeom+i-1) + zr(ideplm+i-1)
!            WORK(100+I) = ZR(IDEPLM+I-1)
                xidepm(i) = zr(ideplm+i-1)
150          continue
        else if ((zk16(icompo+2) (1:5).eq.'PETIT')) then
!          CALL R8INIR(24,0.D0,WORK(101),1)
            call r8inir(60, 0.d0, xidepm, 1)
        else
            do 152 i = 1, 3*nno
!            WORK(100+I) = ZR(IDEPLM+I-1)
                xidepm(i) = zr(ideplm+i-1)
152          continue
        endif
! ----   CALCUL DES FORCES INTERNES BT.SIGMA
!
!           ON PASSE EN PARAMETRES
!           ZR(IGEOM) : GEOMETRIE CONFIG DEBUT PAS
!           ZR(IDEPLM) : DEPLACEMENT
!           ZR(ICONTM) : CONTRAINTE DE CAUCHY DEBUT DE PAS
!           ZR(IVARIM) (DE 2 A 14) : CONTRAINTES DE STABILISATION
!           ON RECUPERE :
!           ZR(IVECTU) : FORCES INTERNES FIN DE PAS
! ----   INTERPOLATION DES COEFFICIENTS EN FONCTION DE LA TEMPERATURE
! ----   ET DU TEMPS
!
        call moytem(fami, npg, 1, '+', tempm,&
                    iret)
        call jevech('PMATERC', 'L', imate)
        nomres(1) = 'E'
        nomres(2) = 'NU'
        nbv = 2
        call rcvalb(famil, kpg, spt, poum, zi(imate),&
                    ' ', 'ELAS', 1, 'TEMP', [tempm],&
                    nbv, nomres, valres, icodre, 1)
        e = valres(1)
        nu = valres(2)
        para(1) = e
        para(2) = nu
        do 557 i = 1, npg
            do 556 j = 1, 6
                sigma(6*(i-1)+j)=zr(icontm+18*(i-1)+j-1)
556          continue
557      continue
!
        if (nomshb .eq. 'SHB8') then
            do 94 i = 1, 12
                fstab(i) = zr(icontm+i-1+6)
94          continue
            call jevech('PVECTUR', 'E', ivectu)
            call sh8for(zr(igeom), para, xidepm, sigma, fstab,&
                        zr(ivectu))
        else if (nomshb.eq.'SHB6') then
            call jevech('PVECTUR', 'E', ivectu)
            call sh6for(zr(igeom), sigma, zr(ivectu))
        else if (nomshb.eq.'SHB15') then
            call jevech('PVECTUR', 'E', ivectu)
            call sh1for(zr(igeom), sigma, zr(ivectu))
        else if (nomshb.eq.'SHB20') then
            call jevech('PVECTUR', 'E', ivectu)
            call sh2for(zr(igeom), sigma, zr(ivectu))
        endif
    endif
end subroutine
