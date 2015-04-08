subroutine te0050(option, nomte)
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
    implicit none
! aslint: disable=W0104
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/lteatt.h"
#include "asterfort/pmfmats.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          OPTION : 'AMOR_MECA'
!                                OU 'RIGI_MECA_HYST'
!        POUR TOUS LES TYPES D'ELEMENTS (SAUF LES ELEMENTS DISCRETS)
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: nbres, nbpar
    parameter         ( nbres=2 )
    parameter         ( nbpar=3 )
!
    integer :: jgano, iret, nbval, nbddl, idimge, npara
    integer :: i, j, k, kns, ks, mater, irigi, imass
    integer :: iresu, imate, ins, irns
    integer :: idresu(5), idrigi(2), idmass(2), idgeo(5)
    integer :: ipoids, ivf, idfdx, igeom
    integer :: ndim, nno, nnos, npg1, ino
!
    real(kind=8) :: alpha, beta, eta, valres(nbres), valpar(nbpar), vxyz
!
    integer :: icodre(nbres)
    character(len=8) :: nompar(nbpar), nomat
    character(len=16) :: nomres(nbres)
    character(len=32) :: phenom
!-----------------------------------------------------------------------------
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
                     npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfdx,jgano=jgano)
!
!     -- RECUPERATION DES CHAMPS PARAMETRES ET DE LEURS LONGUEURS:
!     ------------------------------------------------------------
    ins=0
    irns=0
    if (option .eq. 'AMOR_MECA') then
        call tecach('NNO', 'PRIGIEL', 'L', ins, iad=idrigi(1))
        if (ins .eq. 0) then
            call tecach('ONN', 'PMATUUR', 'E', iret, nval=5,&
                        itab=idresu)
        else
            call tecach('NNN', 'PMATUNS', 'E', irns, nval=5,&
                        itab=idresu)
            if (irns .ne. 0) call tecach('ONN', 'PMATUUR', 'E', iret, 5, itab=idresu)
        endif
    else if (option.eq.'RIGI_MECA_HYST') then
        call tecach('ONN', 'PMATUUC', 'E', iret, nval=5, itab=idresu)
    else
        ASSERT(.false.)
    endif
    nbval= idresu(2)
!
    nompar(1)='X'
    nompar(2)='Y'
    nompar(3)='Z'
!
    call tecach('ONN', 'PGEOMER', 'L', iret, nval=5,&
                itab=idgeo)
    igeom=idgeo(1)
    idimge=idgeo(2)/nno
!
    ASSERT(idimge.eq.2 .or. idimge.eq.3)
!
    npara=idimge
    do k = 1, idimge
        vxyz = 0.d0
        do ino = 1, nno
            vxyz = vxyz+zr(igeom + idimge*(ino-1) +k -1)
        end do
        valpar(k) = vxyz/nno
    end do
!
    call jevech('PMATERC', 'L', imate)
    mater=zi(imate)
    call rccoma(mater, 'ELAS', 0, phenom, icodre(1))
    if(.not.(phenom .eq. 'ELAS'       .or. phenom .eq. 'ELAS_ORTH'&
        .or. phenom .eq. 'ELAS_COQMU' .or. phenom .eq. 'ELAS_GLRC'&
        .or. phenom .eq. 'ELAS_DHRC')) then
        call utmess('F', 'MODELISA10_3', sk=phenom)
    endif

!   -- si l'element est multifibre, il faut prendre le materiau "section"
!      pour recuperer les coefficients de dilatation :
    if (lteatt('TYPMOD2','PMF')) then
        call pmfmats(mater, nomat)
    else
        nomat=' '
    endif
!
    if (ins .eq. 0) then
        call tecach('ONN', 'PRIGIEL', 'L', iret, nval=2,&
                    itab=idrigi)
        ASSERT(idrigi(2).eq.nbval)
    else if (irns.eq.0) then
        call tecach('ONN', 'PRIGINS', 'L', iret, nval=2,&
                    itab=idrigi)
        ASSERT(idrigi(2).eq.nbval)
    endif
!
!     -- RECUPERATION DES COEFFICIENTS FONCTIONS DE LA GEOMETRIE :
!     -------------------------------------------------------------
!
    if (option .eq. 'AMOR_MECA') then
!     --------------------------------
        call tecach('ONN', 'PMASSEL', 'L', iret, nval=2,&
                    itab=idmass)
!
        if (ins .eq. 0) then
            ASSERT(idmass(2).eq.nbval)
        else if (irns.eq.0) then
            nbddl = int(sqrt(dble(nbval)))
            ASSERT(idmass(2).eq. nbddl*(nbddl+1)/2)
        endif
!
        nomres(1)='AMOR_ALPHA'
        nomres(2)='AMOR_BETA'
        valres(1) = 0.d0
        valres(2) = 0.d0
        call rcvalb('RIGI', 1, 1, '+', mater, nomat, phenom, npara, nompar, valpar, 2,&
                    nomres, valres, icodre, 0, nan='NON')
!
    else if (option.eq.'RIGI_MECA_HYST') then
!     ------------------------------------------
        nomres(1)='AMOR_HYST'
        valres(1) = 0.d0
        call rcvalb('RIGI', 1, 1, '+', mater, nomat, phenom, npara, nompar, valpar, 1,&
                    nomres, valres, icodre, 0, nan='NON')
    else
        ASSERT(.false.)
    endif
!
!     -- CALCUL PROPREMENT DIT :
!     --------------------------
    iresu= idresu(1)
    irigi= idrigi(1)
    if (option .eq. 'AMOR_MECA') then
        alpha= valres(1)
        beta = valres(2)
        imass= idmass(1)
!
        if (ins .eq. 0 .or. irns .ne. 0) then
            do i = 1, nbval
                if (irigi .ne. 0) then
                    zr(iresu-1+i)=alpha*zr(irigi-1+i)+beta*zr(imass-1+i)
                else
                    zr(iresu-1+i)=beta*zr(imass-1+i)
                endif
            end do
        else
!     Cas non symetrique
            do i = 1, nbddl
                kns = (i-1)*nbddl
                do j = 1, nbddl
                    if (j .le. i) then
                        ks = (i-1)*i/2+j
                    else
                        ks = (j-1)*j/2+i
                    endif
                    zr(iresu-1+kns+j)=alpha*zr(irigi-1+kns+j) +beta* zr(imass-1+ks)
                end do
            end do
        endif
    else if (option.eq.'RIGI_MECA_HYST') then
        eta = valres(1)
        do i = 1, nbval
            zc(iresu-1+i)=dcmplx(zr(irigi-1+i),eta*zr(irigi-1+i))
        end do
    endif
end subroutine
