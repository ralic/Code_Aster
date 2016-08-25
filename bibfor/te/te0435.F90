subroutine te0435(option, nomte)
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
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/codere.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/jevecd.h"
#include "asterfort/mbxnlr.h"
#include "asterfort/mbgnlr.h"
#include "asterfort/mbcine.h"
#include "asterfort/mbrigi.h"
#include "asterfort/nmprmb_matr.h"
#include "asterfort/r8inir.h"
#include "asterc/r8prem.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/verift.h"
#include "blas/dcopy.h"
!
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL DES OPTIONS DE COMPORTEMENT :
!                                  - FULL_MECA
!                                  - FULL_MECA_ELAS
!                                  - RAPH_MECA
!                                  - RIGI_MECA
!                                  - RIGI_MECA_ELAS
!                                  - RIGI_MECA_TANG
!                                  - RIGI_MECA_IMPLEX
!                                  - RIGI_MECA_PRSU_R
!                          POUR LES MEMBRANES
!    - ARGUMENTS :
!        DONNEES :      OPTION       -->  OPTION DE CALCUL
!                       NOMTE        -->  NOM DU TYPE ELEMENT
! ----------------------------------------------------------------------
!
    character(len=4) :: fami
    integer :: nddl, nno, nnos, npg, ndim, ncomp, nvari
    integer :: n, kpg, iret, cod(9)
    integer :: ipoids, ivf, idfde, jgano, jtab(7)
    integer :: igeom, icacoq, imate, icompo, icarcr
    integer :: iinstm, iinstp, icontm, ideplm, ideplp, ivarim, ivarix
    integer :: ivectu, icontp, ivarip, jcret, imatuu, imatun, icontx, i_pres
    real(kind=8) :: dff(2, 9), alpha, beta, h, preten
    aster_logical :: vecteu, matric
!
! - BOOLEEN UTILES
!
    vecteu = ((option(1:9).eq.'FULL_MECA').or. (option .eq.'RAPH_MECA'))
    matric = ((option(1:9).eq.'FULL_MECA').or. (option(1:9).eq.'RIGI_MECA'))
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
!
    call jevech('PGEOMER', 'L', igeom)
    
! ON UTILISE PCACOQU ET PMATERC POUR TOUT EXCEPTE LA PRESSION SUIVEUSE
    if (option .ne. 'RIGI_MECA_PRSU_R') then
        call jevech('PCACOQU', 'L', icacoq)
        call jevech('PMATERC', 'L', imate)
    endif
!
    if ((option(1:9) .eq.'FULL_MECA').or. (option .eq.'RAPH_MECA')&
    .or. ((option(1:10).eq.'RIGI_MECA_').and.(option .ne. 'RIGI_MECA_PRSU_R'))) then
        call jevech('PCOMPOR', 'L', icompo)
        call jevech('PCARCRI', 'L', icarcr)
!
        call jevech('PINSTMR', 'L', iinstm)
        call jevech('PINSTPR', 'L', iinstp)
        call jevech('PCONTMR', 'L', icontm)
        call tecach('OOO', 'PVARIMR', 'L', iret, nval=7,&
                    itab=jtab)
        nvari = max(jtab(6),1)*jtab(7)
        call jevech('PVARIMR', 'L', ivarim)
        call jevech('PVARIMP', 'L', ivarix)
    endif
    
    if (option .ne. 'RIGI_MECA') then
        call jevech('PDEPLMR', 'L', ideplm)
        call jevech('PDEPLPR', 'L', ideplp)
    endif
    
! PARAMETRES NECESSAIRE AU CALCUL DE LA MATRICE DE RIGITE POUR PRESSION SUIVEUSE
    if (option.eq.'RIGI_MECA_PRSU_R') then
        call jevecd('PPRESSR', i_pres, 0.d0)
    endif
!
! - PARAMETRES EN SORTIE
!
    if ((option(1:9).eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
        call jevech('PCODRET', 'E', jcret)
!       ESTIMATION VARIABLES INTERNES A L'ITERATION PRECEDENTE
        call dcopy(npg*nvari, zr(ivarix), 1, zr(ivarip), 1)
    endif
!    

    if ((option(1:9) .eq.'FULL_MECA').or.((option(1:9).eq.'RIGI_MECA').and.&
        (option .ne. 'RIGI_MECA_PRSU_R'))) then
        call jevech('PMATUUR', 'E', imatuu)
    endif
    
    if (option .eq. 'RIGI_MECA_PRSU_R') then
        call jevech('PMATUNS', 'E', imatun)
    endif
 
!
! - PARAMETRES EN SORTIE SUPPLEMENTAIRE POUR LA METHODE IMPLEX
    if (option .eq. 'RIGI_MECA_IMPLEX') then
        call jevech('PCONTXR', 'E', icontx)
! ------ INITIALISATION DE LA CONTRAINTE INTERPOLE CONTX=CONTM
        call dcopy(npg*ncomp, zr(icontm), 1, zr(icontx), 1)
    endif  
!    
!
!
! ------------------------------------------------------------------------------------
!
!
! - INITIALISATION CODES RETOURS
!
    do kpg = 1, npg
        cod(kpg)=0
    end do
!
! - DIRECTION DE REFERENCE POUR UN COMPORTEMENT ANISOTROPE
!
    alpha = zr(icacoq+1) * r8dgrd()
    beta = zr(icacoq+2) * r8dgrd()
    
! - EPAISSEUR ET PRECONTRAINTES
!
    h = zr(icacoq) 
    preten = zr(icacoq+3)
    
!
! - DEBUT DE LA BOUCLE SUR LES POINTS DE GAUSS
!
    do kpg = 1, npg
!
!
! ! --- MISE SOUS FORME DE TABLEAU DES VALEURS ET DES DERIVEES
!     DES FONCTIONS DE FORME
!
        do n = 1, nno
            dff(1,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2)
            dff(2,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2+1)
        end do
!
! ---   ON DISTINGUE LES PETITS ET GRANDS DEFORMATIONS  
!
        if (option.eq.'RIGI_MECA') then 
            call mbxnlr(option,fami,nddl,nno,ncomp,kpg,ipoids,igeom,&
                  imate,ideplm,ideplp,ivectu,icontp,&
                  imatuu,dff,alpha,beta,&
                  vecteu,matric)
!
        elseif (option.eq.'RIGI_MECA_PRSU_R') then 
        
            call nmprmb_matr(nno, npg, kpg, zr(ipoids+kpg), zr(ivf), dff,&
                             igeom,ideplm,ideplp,i_pres, imatun)
!
        elseif ((option(1:9) .eq.'FULL_MECA').or. (option .eq.'RAPH_MECA')&
                                  .or. (option(1:10).eq.'RIGI_MECA_')) then
            
            if (zk16(icompo + 2) .eq. 'PETIT') then
                call mbxnlr(option,fami,nddl,nno,ncomp,kpg,ipoids,igeom,&
                      imate,ideplm,ideplp,ivectu,icontp,&
                      imatuu,dff,alpha,beta,&
                      vecteu,matric)
        
            elseif (zk16 ( icompo + 2 )(1:9) .eq. 'GROT_GDEP') then
!
                if (zk16 ( icompo ) ( 1 : 14 ) .eq. 'ELAS_HYP_MEMB_') then
!
                    if ((abs(alpha).gt.r8prem()) .or. (abs(beta).gt.r8prem())) then
                        call utmess('A', 'MEMBRANE_6')
                    endif
!
                    call mbgnlr(option,vecteu,matric,nno,ncomp,imate,icompo,dff,alpha,beta,&
                  h,preten,igeom,ideplm,ideplp,kpg,fami,ipoids,icontp,ivectu,imatuu)
!
                else
                    call utmess('F', 'MEMBRANE_3')
                endif
!
            else
! --------- AUTRE MESURE DE DEFORMATION
               call utmess('F', 'MEMBRANE_2', sk=zk16(icompo+2))
            endif
!        
        endif
!
!
! - FIN DE LA BOUCLE SUR LES POINTS DE GAUSS
    end do
!
    if ((option(1:9).eq.'FULL_MECA') .or. (option(1:9).eq.'RAPH_MECA')) then
        call codere(cod, npg, zi(jcret))
    endif
!
end subroutine
