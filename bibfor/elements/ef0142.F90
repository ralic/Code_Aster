subroutine ef0142(nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/moytem.h"
#include "asterfort/pmfrig.h"
#include "asterfort/poefgr.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/porigi.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rhoequ.h"
#include "asterfort/utmess.h"
#include "asterfort/vecma.h"
    character(len=16) :: nomte
!     ------------------------------------------------------------------
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
!     CALCUL DE EFGE_ELNO
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, jeffo, labsc, lmater, lopt, nbpar
    integer :: nbref, nbres
    real(kind=8) :: absmoy, cm, phie, phii, rhofe, rhofi, rhos
    real(kind=8) :: valpar
!-----------------------------------------------------------------------
    parameter(nbres=3,nbref=6)
    real(kind=8) :: valres(nbres), valref(nbref)
    integer :: codres(nbres), codref(nbref)
    character(len=8) :: nompar
    character(len=16) :: nomres(nbres), nomref(nbref)
    real(kind=8) :: zero, e, nu, rho
    real(kind=8) :: klv(78), klc(12, 12)
    character(len=24) :: suropt
    integer :: iret
!     ------------------------------------------------------------------
    data nomres/'E','NU','RHO'/
    data nomref/'E','NU','RHO','PROF_RHO_F_INT','PROF_RHO_F_EXT','COEF_MASS_AJOU'/
!     --------------------------------------------------
    integer, parameter :: nb_cara1 = 2
    real(kind=8) :: vale_cara1(nb_cara1)
    character(len=8) :: noms_cara1(nb_cara1)
    data noms_cara1 /'R1','EP1'/
!-----------------------------------------------------------------------
    zero=0.d0
!
!     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
!
    call jevech('PMATERC', 'L', lmater)
    call moytem('NOEU', 2, 1, '+', valpar,&
                iret)
    nompar='TEMP'
    nbpar=1
!
    call jevech('PSUROPT', 'L', lopt)
    suropt=zk24(lopt)
    if (suropt .eq. 'MASS_FLUI_STRU') then
        call jevech('PABSCUR', 'L', labsc)
        call poutre_modloc('CAGEP1', noms_cara1, nb_cara1, lvaleur=vale_cara1)
        absmoy=(zr(labsc-1+1)+zr(labsc-1+2))/2.d0
        call rcvalb('NOEU', 1, 1, '+', zi(lmater),&
                    ' ', 'ELAS_FLUI', 1, 'ABSC', [absmoy],&
                    nbref, nomref, valref, codref, 1)
        e=valref(1)
        nu=valref(2)
        rhos=valref(3)
        rhofi=valref(4)
        rhofe=valref(5)
        cm=valref(6)
        phie = vale_cara1(1)*2.d0
        if (phie .eq. 0.d0) then
            call utmess('F', 'ELEMENTS3_26')
        endif
        phii=(phie-2.d0*vale_cara1(2))
        call rhoequ(rho, rhos, rhofi, rhofe, cm,&
                    phii, phie)
!
    else
        if (nomte .ne. 'MECA_POU_D_EM') then
            call rcvalb('NOEU', 1, 1, '+', zi(lmater),&
                        ' ', 'ELAS', nbpar, nompar, [valpar],&
                        2, nomres, valres, codres, 1)
            call rcvalb('NOEU', 1, 1, '+', zi(lmater),&
                        ' ', 'ELAS', nbpar, nompar, [valpar],&
                        1, nomres(3), valres(3), codres(3), 0)
            if (codres(3) .ne. 0) valres(3)=zero
            e=valres(1)
            nu=valres(2)
            rho=valres(3)
        endif
    endif
!
!     --- CALCUL DE LA MATRICE DE RIGIDITE LOCALE ---
!
    if (nomte .eq. 'MECA_POU_D_EM') then
        call pmfrig(nomte, zi(lmater), klv)
    else
        call porigi(nomte, e, nu, -1.d0, klv)
    endif
!
!     ---- MATRICE RIGIDITE LIGNE > MATRICE RIGIDITE CARRE
!
    call vecma(klv, 78, klc, 12)
!
!
    call jevech('PEFFORR', 'E', jeffo)
    call poefgr(nomte, klc, zi(lmater), e, nu,&
                rho, zr(jeffo))
    do i = 1, 6
        zr(jeffo+i-1)=-zr(jeffo+i-1)
        zr(jeffo+i+6-1)=zr(jeffo+i+6-1)
    end do
!
end subroutine
