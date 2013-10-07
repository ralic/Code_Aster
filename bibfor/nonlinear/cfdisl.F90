function cfdisl(deficz, questz)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/mminfl.h"
    logical :: cfdisl
    character(len=*) :: deficz
    character(len=*) :: questz
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES)
!
! RETOURNE DES INFOS DIVERSES POUR LE CONTACT (LOGICAL)
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO  : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
! IN  QUESTI  : QUESTION (PARAMETRE INTERROGE)
!
! ----------------------------------------------------------------------
!
    character(len=24) :: defico, questi
    integer :: iform
    integer :: icont, ifrot, ndimg, izone
!
! ----------------------------------------------------------------------
!
!
! --- INITIALISATIONS
!
    defico = deficz
    questi = questz
    cfdisl = .false.
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    if (questi .eq. 'MODI_MATR_GLOB') then
        iform = cfdisi(defico,'FORMULATION')
        if (iform .eq. 1) then
            icont = cfdisi(defico,'ALGO_CONT')
            ifrot = cfdisi(defico,'ALGO_FROT')
            ndimg = cfdisi(defico,'NDIM')
            if (icont .eq. 4) then
                cfdisl = .true.
            endif
            if (ifrot .ne. 0) then
                if (ifrot .eq. 2) then
                    if (ndimg .eq. 2) then
                        cfdisl = .false.
                    else
                        cfdisl = .true.
                    endif
                else
                    cfdisl = .true.
                endif
            endif
        endif
!
    else if (questi .eq.'MATR_CONT') then
        icont = cfdisi(defico,'ALGO_CONT')
        cfdisl = ((icont.eq.1).or.(icont.eq.5))
!
    else if (questi .eq.'LISSAGE') then
        cfdisl = cfdisi(defico,'LISSAGE').eq.1
!
    else if (questi .eq.'COEF_ADAPT') then
        cfdisl = cfdisi(defico,'COEF_ADAPT').eq.1
!
    else if (questi .eq.'CONT_DISC_GLIS') then
        izone = 1
        cfdisl = mminfl(defico,'GLISSIERE_ZONE',izone )
!
    else if (questi .eq.'CONT_XFEM_GG') then
        cfdisl = cfdisi(defico,'CONT_XFEM_GG').eq.1
!
    else if (questi .eq.'EXIS_XFEM_CZM') then
        cfdisl = cfdisi(defico,'EXIS_XFEM_CZM').eq.1
!
    else if (questi .eq.'EXIS_PENA') then
        cfdisl = cfdisi(defico,'EXIS_PENA').eq.1
!
    else if (questi .eq.'ALL_VERIF') then
        cfdisl = cfdisi(defico,'ALL_VERIF').eq.1
!
    else if (questi .eq.'EXIS_VERIF') then
        cfdisl = cfdisi(defico,'EXIS_VERIF').eq.1
!
    else if (questi .eq.'EXIS_GLISSIERE') then
        cfdisl = cfdisi(defico,'EXIS_GLISSIERE').eq.1
!
    else if (questi .eq.'ALL_INTEG_NOEUD') then
        cfdisl = cfdisi(defico,'ALL_INTEG_NOEUD').eq.1
!
    else if (questi .eq.'ALL_INTERPENETRE') then
        cfdisl = cfdisi(defico,'ALL_INTERPENETRE').eq.1
!
    else if (questi .eq.'STOP_INTERP') then
        cfdisl = cfdisi(defico,'STOP_INTERP').eq.1
!
    else if (questi .eq.'FORMUL_MAILLEE') then
        iform = cfdisi(defico,'FORMULATION')
        cfdisl = (iform.eq.1).or.(iform.eq.2)
!
    else if (questi .eq.'FORMUL_DISCRETE') then
        iform = cfdisi(defico,'FORMULATION')
        cfdisl = (iform.eq.1)
!
    else if (questi .eq.'FORMUL_CONTINUE') then
        iform = cfdisi(defico,'FORMULATION')
        cfdisl = (iform.eq.2)
!
    else if (questi .eq.'FORMUL_XFEM') then
        iform = cfdisi(defico,'FORMULATION')
        cfdisl = (iform.eq.3)
!
    else if (questi.eq.'AXISYMETRIQUE') then
        cfdisl = cfdisi(defico,'AXISYMETRIQUE').eq.1
!
    else if (questi.eq.'FROTTEMENT') then
        ifrot = cfdisi(defico,'ALGO_FROT')
        cfdisl = ifrot.ne.0
!
    else if (questi.eq.'FROT_DISCRET') then
        ifrot = cfdisi(defico,'ALGO_FROT')
        cfdisl = ((ifrot.eq.1).or.(ifrot.eq.2))
!
    else if (questi.eq.'FROT_PENA') then
        ifrot = cfdisi(defico,'ALGO_FROT')
        cfdisl = ifrot.eq.1
!
    else if (questi.eq.'FROT_LAGR') then
        ifrot = cfdisi(defico,'ALGO_FROT')
        cfdisl = ifrot.eq.2
!
    else if (questi.eq.'FROT_LAGR_2D') then
        icont = cfdisi(defico,'ALGO_CONT')
        ifrot = cfdisi(defico,'ALGO_FROT')
        ndimg = cfdisi(defico,'NDIM' )
        if ((icont.eq.5) .and. (ifrot.eq.2) .and. (ndimg.eq.2)) then
            cfdisl = .true.
        else
            cfdisl = .false.
        endif
!
    else if (questi.eq.'CONT_PENA') then
        icont = cfdisi(defico,'ALGO_CONT')
        cfdisl = icont.eq.4
!
    else if (questi.eq.'CONT_LAGR') then
        icont = cfdisi(defico,'ALGO_CONT')
        cfdisl = icont.eq.5
!
    else if (questi.eq.'CONT_ACTI') then
        icont = cfdisi(defico,'ALGO_CONT')
        cfdisl = icont.eq.1
!
    else if (questi.eq.'CONT_GCP') then
        icont = cfdisi(defico,'ALGO_CONT')
        cfdisl = icont.eq.2
!
    else if (questi.eq.'PRE_COND_DIRICHLET') then
        cfdisl = cfdisi(defico,'PRE_COND').eq.1
!
    else if (questi.eq.'FROT_3D') then
        ifrot = cfdisi(defico,'ALGO_FROT')
        ndimg = cfdisi(defico,'NDIM' )
        if (ifrot .eq. 1) then
            cfdisl = .true.
        else if (ifrot.eq.2) then
            cfdisl = ndimg.eq.3
        else if (ifrot.eq.0) then
            cfdisl = .false.
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'GEOM_NEWTON') then
        cfdisl = cfdisi(defico,'ALGO_RESO_GEOM').eq.1
!
    else if (questi.eq.'FROT_NEWTON') then
        cfdisl = cfdisi(defico,'ALGO_RESO_FROT').eq.1
!
    else if (questi.eq.'CONT_NEWTON') then
        cfdisl = cfdisi(defico,'ALGO_RESO_CONT').eq.1
!
    else if (questi.eq.'GEOM_BOUCLE') then
        cfdisl = cfdisi(defico,'ALGO_RESO_GEOM') .eq. 0 .and. cfdisi( defico,'NB_ITER_GEOM') .ne.&
                 0
!
    else if (questi.eq.'CONT_BOUCLE') then
        cfdisl = cfdisi(defico,'ALGO_RESO_CONT').eq.0
!
    else if (questi.eq.'FROT_BOUCLE') then
        cfdisl = cfdisi(defico,'ALGO_RESO_FROT').eq.0
!
    else if (questi.eq.'REAC_GEOM_SANS') then
        cfdisl = cfdisi(defico,'NB_ITER_GEOM').eq.0
!
    else if (questi.eq.'REAC_GEOM_MANU') then
        cfdisl = cfdisi(defico,'NB_ITER_GEOM').gt.0
!
    else if (questi.eq.'REAC_GEOM_AUTO') then
        cfdisl = cfdisi(defico,'NB_ITER_GEOM').lt.0
!
    else
        ASSERT(.false.)
    endif
!
end function
