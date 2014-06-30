subroutine surfc1(char, ifm)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfdisr.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mminfl.h"
#include "asterfort/mminfr.h"
    character(len=8) :: char
    integer :: ifm
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - AFFICHAGE DONNEES)
!
! AFFICHAGE LES INFOS CONTENUES DANS LA SD CONTACT POUR LA FORMULATION
! DISCRETE
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  IFM    : UNITE D'IMPRESSION
!
!
!
!
    integer :: nzoco
    integer :: izone
    character(len=24) :: defico
    logical(kind=1) :: lveri, lgcp, lgliss
    integer :: isto, lgbloc, gcpmax, gcppre, gcprec
    real(kind=8) :: tolint, precis, gcpres, aljeu
    real(kind=8) :: coefpn, coefpt, coefff, coefte
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
!
! --- INITIALISATIONS
!
    nzoco = cfdisi(defico,'NZOCO')
    isto = cfdisi(defico,'STOP_SINGULIER')
    lgbloc = cfdisi(defico,'NB_RESOL')
    lgcp = cfdisl(defico,'CONT_GCP' )
    precis = cfdisr(defico,'RESI_ABSO')
    gcpmax = cfdisi(defico,'ITER_GCP_MAXI')
    gcppre = cfdisi(defico,'PRE_COND')
    gcpres = cfdisr(defico,'COEF_RESI')
    gcprec = cfdisi(defico,'RECH_LINEAIRE')
    lgliss = cfdisl(defico,'CONT_DISC_GLIS')
    aljeu = cfdisr(defico,'ALARME_JEU')
!
! --- IMPRESSIONS POUR L'UTILISATEUR
!
    write (ifm,*)
    write (ifm,*) '<CONTACT> INFOS SPECIFIQUES SUR LA FORMULATION'//&
     &              ' DISCRETE'
    write (ifm,*)
!
! --- IMPRESSIONS POUR LES PARAMETRES CONSTANTS
!
    write (ifm,*) '<CONTACT> ... PARAMETRES CONSTANTS SUR TOUTES '//&
     &              ' LES ZONES'
!
    write (ifm,1070) 'STOP_SINGULIER  ',isto
    write (ifm,1070) 'NB_RESOL        ',lgbloc
    if (lgliss) then
        write (ifm,*) '<CONTACT> ...... GLISSIERE : OUI'
    else
        write (ifm,*) '<CONTACT> ...... GLISSIERE : NON'
    endif
    if (lgliss) then
        write (ifm,1071) 'ALARME_JEU     ',aljeu
    endif
!
    if (lgcp) then
        write (ifm,1070) 'PRE_COND        ',gcppre
        write (ifm,1070) 'RECH_LINEAIRE   ',gcprec
        write (ifm,1071) 'RESI_ABSO       ',precis
        write (ifm,1070) 'ITER_GCP_MAXI   ',gcpmax
        write (ifm,1071) 'COEF_RESI       ',gcpres
    endif
!
! --- IMPRESSIONS POUR LES PARAMETRES VARIABLES
!
    write (ifm,*) '<CONTACT> ... PARAMETRES VARIABLES / ZONE'
    do 50 izone = 1, nzoco
        write (ifm,*) '<CONTACT> ...... ZONE : ',izone
        lveri = mminfl(defico,'VERIF',izone )
        if (lveri) then
            write (ifm,*) '<CONTACT> ...... ZONE DE VERIFICATION'
            tolint = mminfr(defico,'TOLE_INTERP',izone)
            write (ifm,1071) 'TOLE_INTERP     ',tolint
        else
            write (ifm,*) '<CONTACT> ...... ZONE DE CALCUL'
            coefpn = mminfr(defico,'E_N',izone)
            coefpt = mminfr(defico,'E_T',izone)
            coefff = mminfr(defico,'COEF_COULOMB',izone)
            coefte = mminfr(defico,'COEF_MATR_FROT',izone)
            write (ifm,1071) 'COEF_MATR_FROT  ',coefte
            write (ifm,1071) 'E_N             ',coefpn
            write (ifm,1071) 'E_T             ',coefpt
            write (ifm,1071) 'COULOMB         ',coefff
        endif
50  end do
!
    1070 format (' <CONTACT> ...... PARAM. : ',a16,' - VAL. : ',i5)
    1071 format (' <CONTACT> ...... PARAM. : ',a16,' - VAL. : ',e12.5)
!
    call jedema()
end subroutine
