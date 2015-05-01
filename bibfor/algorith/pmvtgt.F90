subroutine pmvtgt(option, carcri, deps2, sigp, vip,&
                  nbvari, epsilo, varia, matper, dsidep,&
                  smatr, sdeps, ssigp, svip, iret)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-michel.proix at edf.fr
!-----------------------------------------------------------------------
!     OPERATEUR CALC_POINT_MAT : MATRICE TANGENTE PAR PERTURBATION
!     RESSEMBLE A TGVERI MAIS SANS ELEMENT FINI
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
! VAR OPTION NOM DE L'OPTION DE CALCUL
!             IN  : CELLE UTILISEE PAR CALC_POINT_MAT
!             OUT : 'RAPH_MECA' SI BOUCLE, 'FULL_MECA' SI FIN DE BOUCLE
! IN  CARCRI  : CARCRI(1) = type de matrice tangente
!               0 : ANALYTIQUE, on ne passe pas ici
!               1 : PERTURBATION, on calcule Ktgte (FULL_MECA)
!               2 : VERIFICATION, on calcule Ktgte (FULL_MECA) + Kpertu
!               CARCRI(7) = valeur de la perturbation
! IN  DEPS2   : DEFORMATIONS
! IN  SIGP    : CONTRAINTES
! IN  VIP     : VARIABLES INTERNES
! IN  NBVARI  : Nombre de variables internes
! VAR EPSILO  : VALEUR DE LA PERTURBATION, A GARDER
! VAR VARIA   : TABLEAU DES VARIATIONS
! VAR MATPER  : MATRICE TANGENTE PAR PERTURBATION
! VAR SMATR   : SAUVEGARDE MATRICE TANGENTE
! VAR SDEPS   : SAUVEGARDE DEFORMATIONS
! VAR SSIGP   : SAUVEGARDE CONTRAINTES
! VAR SVIP    : VARIABLES INTERNES
! OUT IRET  SI IRET = 0 -> FIN, SINON -> BOUCLE
! ----------------------------------------------------------------------
    implicit none
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
    character(len=16) :: option
    integer :: iret, nbvari
    real(kind=8) :: carcri(*), deps2(6), sigp(6), matper(36), dsidep(6, 6)
    real(kind=8) :: sdeps(6), ssigp(6), vip(nbvari), svip(nbvari), smatr(36)
    real(kind=8) :: varia(2*36)
!
!
    character(len=24) :: matra, matrc
    integer :: ematra, ematrc, exi, i, j, indi, nvar, init, pos
    real(kind=8) :: v, epsilo, fp, fm, pertu, maxeps
    save init,pos
    data matra  /'PYTHON.TANGENT.MATA'/
    data matrc  /'PYTHON.TANGENT.MATC'/
    data init,pos /1,0/
! ----------------------------------------------------------------------
!
    call jemarq()
!
!     Calcul de la matrice TGTE par PERTURBATION
!
    iret=0
    if (abs(carcri(2)) .lt. 0.1d0) then
        goto 9999
    endif
    if (option(1:9) .eq. 'RIGI_MECA') then
        goto 9999
    endif
!
! --  INITIALISATION (PREMIER APPEL)
!
    if (init .eq. 1) then
!
!       PERTURBATION OU VERIFICATION => FULL_MECA
        if (option .ne. 'FULL_MECA') then
            goto 9999
        endif
!
!       CALCUL de la valeur de la perturbation
        maxeps=0.d0
        do 555 i = 1, 6
            maxeps=max(maxeps,abs(deps2(i)))
555      continue
        pertu=carcri(7)
        epsilo=pertu*maxeps
        if (epsilo .lt. r8miem()) then
            call utmess('A', 'ALGORITH11_86')
            goto 9999
        endif
!
!      ARCHIVAGE DES VALEURS DE REFERENCE
        call dcopy(6, deps2, 1, sdeps, 1)
        call dcopy(6, sigp, 1, ssigp, 1)
        call dcopy(nbvari, vip, 1, svip, 1)
!       ARCHIVAGE DE LA MATRICE TANGENTE COHERENTE
        call dcopy(36, dsidep, 1, smatr, 1)
!      PREPARATION DES ITERATIONS
        option = 'RAPH_MECA'
        iret = 1
        init = 0
        pos = 0
!
    endif
!
! -- TRAITEMENT DES VARIATIONS
!
!
!    SAUVEGARDE DE LA FORCE INTERIEURE PERTURBEE
!
    nvar = int((pos+1)/2)
!
    if (nvar .gt. 0) then
        call dcopy(6, sigp, 1, varia(1+(pos-1)*6), 1)
    endif
!
    pos = pos + 1
    nvar = int((pos+1)/2)
    indi = 1-2*mod(pos,2)
!
    if (nvar .le. 6) then
        call dcopy(6, sdeps, 1, deps2, 1)
        deps2(nvar) = sdeps(nvar) + indi*epsilo
!
!      INITIALISATION DES CHAMPS 'E'
        call r8inir(6, 0.d0, sigp, 1)
        iret=1
        goto 9999
    endif
!
!    CALCUL DE LA MATRICE TANGENTE
!
    do 559 i = 1, 6
        do 560 j = 1, 6
            fm = varia((2*j-2)*6+i)
            fp = varia((2*j-1)*6+i)
            v = (fp-fm)/(2*epsilo)
            matper((i-1)*6+j) = v
560      continue
559  end do
!
!    MENAGE POUR ARRET DE LA ROUTINE
!
    iret = 0
    init = 1
    option = 'FULL_MECA'
!
!    RETABLISSEMENT DE LA SOLUTION
    call dcopy(6, sdeps, 1, deps2, 1)
    call dcopy(6, ssigp, 1, sigp, 1)
    call dcopy(nbvari, svip, 1, vip, 1)
!
!     PERTURBATION => SAUVEGARDE DE LA MATRICE CALCULEE PAR
!     DIFFERENCES FINIES COMME MATRICE TANGENTE
!
    if (abs(carcri(2)-1.d0) .lt. 0.1d0) then
        call dcopy(36, matper, 1, dsidep, 1)
!
!     VERIFICATION
!
    else if (abs(carcri(2)-2.d0).lt.0.1d0) then
        call dcopy(36, smatr, 1, dsidep, 1)
!
!      CREATION DES OBJETS
!      CE N'EST PAS LA PREMIERE FOIS QU'ON CALCULE LA MATRICE TANGENTE
!      -> ON NE CONSERVE QUE LE DERNIER CALCUL (EN COURS)
        call jeexin(matra, exi)
        if (exi .ne. 0) then
            call jedetr(matra)
            call jedetr(matrc)
        endif
        call wkvect(matra, 'G V R', 36, ematra)
        call wkvect(matrc, 'G V R', 36, ematrc)
        call dcopy(36, smatr, 1, zr(ematra), 1)
        call dcopy(36, matper, 1, zr(ematrc), 1)
!         CALL JELIBE(MATRA)
!         CALL JELIBE(MATRC)
    endif
!
9999  continue
!
    call jedema()
end subroutine
