subroutine te0434(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W0104
    implicit none
#include "asterfort/assert.h"
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/mbgchg.h"
#include "asterfort/mbxchg.h"
#include "asterfort/rccoma.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL DES OPTIONS DE CHARGEMENT :
!                                  - CHAR_MECA_EPSI_R
!                                  - CHAR_MECA_EPSI_F
!                                  - CHAR_MECA_PESA_R
!                                  - CHAR_MECA_TEMP_R
!                                  - FORC_NODA
!                                  - REFE_FORC_NODA
!                          POUR LES MEMBRANES
!    - ARGUMENTS :
!        DONNEES :      OPTION       -->  OPTION DE CALCUL
!                       NOMTE        -->  NOM DU TYPE ELEMENT
! ----------------------------------------------------------------------
!
    character(len=4) :: fami
    character(len=32) :: phenom
    integer :: nddl, nno, nnos, npg, ndim, ncomp
    integer :: n, kpg
    integer :: ipoids, ivf, idfde, jgano, iret, icompo,itab(1), itemps
    integer :: igeom, icacoq, imate, icontm, ipesa, iepsin, ivectu
    integer :: icodre1, icodre2
    real(kind=8) :: dff(2, 9), vff(9)
    real(kind=8) :: alpha, beta, h, preten
    aster_logical :: grav
!
!
! -----------------------------------------------------------------
! ---              INITIALISATION DES VARIABLES                 ---
! -----------------------------------------------------------------
!
! - NOMBRE DE COMPOSANTES DES TENSEURS
!
    ncomp = 3
    nddl = 3
!
! - FONCTIONS DE FORME ET POINTS DE GAUSS
!
    fami = 'RIGI'
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
!
! - PARAMETRES EN ENTREE
! - grav : permet d'utiliser PESANTEUR en STAT_NON_LINE
    grav = (option.eq.'CHAR_MECA_PESA_R')
    
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCACOQU', 'L', icacoq)
    
    call tecach('N','PCOMPOR', 'L',iret ,1 , itab)
    icompo = itab(1)
!
    if (option .eq. 'FORC_NODA') then
        call jevech('PCONTMR', 'L', icontm)
        call jevech('PMATERC', 'L', imate)
!
    else if (option.eq.'REFE_FORC_NODA') then
        call jevech('PMATERC', 'L', imate)
!
    else if (option.eq.'CHAR_MECA_EPSI_R') then
        call jevech('PMATERC', 'L', imate)
        call jevech('PEPSINR', 'L', iepsin)
!
    else if (option.eq.'CHAR_MECA_EPSI_F') then
        call jevech('PMATERC', 'L', imate)
        call jevech('PEPSINF', 'L', iepsin)
        call jevech('PTEMPSR', 'L', itemps)
!
    else if (grav) then
        call jevech('PMATERC', 'L', imate)
        call jevech('PPESANR', 'L', ipesa)
!
    else if (option.eq.'CHAR_MECA_TEMP_R') then
        call jevech('PMATERC', 'L', imate)
!
    endif
!
! - PARAMETRES EN SORTIE
!
    call jevech('PVECTUR', 'E', ivectu)
!
! - DIRECTION DE REFERENCE POUR UN COMPORTEMENT ANISOTROPE
!
    alpha = zr(icacoq+1) * r8dgrd()
    beta = zr(icacoq+2) * r8dgrd()
    
! - EPAISSEUR ET PRETCONTRAINTES
    h = zr(icacoq) 
    preten = zr(icacoq+3)/h
!
! -----------------------------------------------------------------
! ---  VERIFICATION DE LA CORRESPONDANCE MATERIAU / COMPORTMENT ---
! -----------------------------------------------------------------
!
    call rccoma(zi(imate), 'ELAS_MEMBRANE', 0, phenom, icodre1)
    call rccoma(zi(imate), 'ELAS', 0, phenom, icodre2)
    
    if (icodre1 .eq. 0) then
        if ((icompo.ne.0) .and. (zk16( icompo + 2 )(1:5) .ne. 'PETIT')) then
            call utmess('F', 'MEMBRANE_10')
        endif
    elseif (icodre2 .eq. 0) then
        if (((icompo.eq.0) .or. (zk16( icompo + 2 )(1:9) .ne. 'GROT_GDEP')) &
              .and. (.not.grav)) then
            call utmess('F', 'MEMBRANE_10')
        endif
    endif
!
! -----------------------------------------------------------------
! ---       DEBUT DE LA BOUCLE SUR LES POINTS DE GAUSS          ---
! -----------------------------------------------------------------
!
    do kpg = 1, npg
!
! --- MISE SOUS FORME DE TABLEAU DES VALEURS ET DES DERIVEES
!     DES FONCTIONS DE FORME
!
        do n = 1, nno
            vff(n) =zr(ivf+(kpg-1)*nno+n-1)
            dff(1,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2)
            dff(2,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2+1)
        end do
        
        if (icodre1 .eq. 0) then

            call mbxchg(option,fami,nddl,nno,ncomp,kpg, npg,iepsin,itemps,ipoids,igeom,&
                  imate,ipesa,ivectu,icontm,vff,dff,alpha,beta)
                  
        elseif (icodre2 .eq. 0) then
        
            if ((option.ne.'FORC_NODA') .and. (option.ne.'CHAR_MECA_PESA_R')) then
                call utmess('F', 'MEMBRANE_7')
            endif
            call mbgchg(option,fami,nddl,nno,ncomp,kpg,imate,icontm,&
              ipoids,ipesa,igeom,ivectu,vff,dff,h,alpha,beta,preten)
              
        endif
    end do
!
! - FIN DE LA BOUCLE SUR LES POINTS DE GAUSS
!
end subroutine
