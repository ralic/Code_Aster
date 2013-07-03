subroutine surfcp(char, ifm)
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
    implicit    none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisr.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    character(len=8) :: char
    integer :: ifm
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - AFFICHAGE DONNEES)
!
! AFFICHAGE LES INFOS CONTENUES DANS LA SD CONTACT POUR TOUTES LES
! FORMULATIONS
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  IFM    : UNITE D'IMPRESSION
!
! ----------------------------------------------------------------------
!
    integer :: iform, icont, ifrot
    character(len=24) :: defico
    integer :: reacca, reacbs, reacbg, reacmx
    integer :: nbreag
    real(kind=8) :: resige, resifr
    integer :: iresoc, iresof, iresog
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
    iform = cfdisi(defico,'FORMULATION')
    icont = cfdisi(defico,'ALGO_CONT')
    ifrot = cfdisi(defico,'ALGO_FROT')
    iresoc = cfdisi(defico,'ALGO_RESO_CONT')
    iresof = cfdisi(defico,'ALGO_RESO_FROT')
    iresog = cfdisi(defico,'ALGO_RESO_GEOM')
!
    nbreag = cfdisi(defico,'NB_ITER_GEOM')
    reacbg = cfdisi(defico,'ITER_GEOM_MAXI')
    resige = cfdisr(defico,'RESI_GEOM')
!
    reacca = cfdisi(defico,'ITER_CONT_MULT')
    reacmx = cfdisi(defico,'ITER_CONT_MAXI')
!
    reacbs = cfdisi(defico,'ITER_FROT_MAXI')
    resifr = cfdisr(defico,'RESI_FROT')
!
! --- IMPRESSIONS POUR L'UTILISATEUR
!
    write (ifm,*)
    write (ifm,*) '<CONTACT> INFOS GENERALES'
    write (ifm,*)
!
! --- FORMULATION
!
    write (ifm,*) '<CONTACT> FORMULATION'
    if (iform .eq. 1) then
        write (ifm,*) '<CONTACT> ... FORMULATION DISCRETE (MAILLEE)'
    else if (iform.eq.2) then
        write (ifm,*) '<CONTACT> ... FORMULATION CONTINUE (MAILLEE)'
    else if (iform.eq.3) then
        write (ifm,*) '<CONTACT> ... FORMULATION XFEM (NON MAILLEE)'
    else
        call assert(.false.)
    endif
!
! --- ALGORITHMES
!
    write (ifm,*) '<CONTACT> MODELE'
    write (ifm,1070) 'ALGO_CONT       ',icont
    write (ifm,1070) 'ALGO_FROT       ',ifrot
!
! --- GEOMETRIE
!
    write (ifm,*) '<CONTACT> ALGORITHMES'
    if (iresog .eq. 0) then
        write (ifm,*) '<CONTACT> ... ALGO. GEOMETRIQUE - POINT FIXE'
        if (nbreag .eq. 0) then
            write (ifm,*) '<CONTACT> ...... PAS DE REAC. GEOM.'
        else if (nbreag.eq.-1) then
            write (ifm,*) '<CONTACT> ...... REAC. GEOM. AUTO.'
        else
            write (ifm,*) '<CONTACT> ...... REAC. GEOM. MANUEL: ',&
            nbreag
        endif
        write (ifm,1070) 'ITER_GEOM_MAXI  ',reacbg
        write (ifm,1071) 'RESI_GEOM       ',resige
        write (ifm,1070) 'NB_ITER_GEOM    ',nbreag
    else if (iresog.eq.1) then
        write (ifm,*) '<CONTACT> ... ALGO. GEOMETRIQUE - NEWTON'
        write (ifm,1071) 'RESI_GEOM       ',resige
    else
        call assert(.false.)
    endif
!
! --- FROTTEMENT
!
    if (iresof .eq. 0) then
        write (ifm,*) '<CONTACT> ... ALGO. FROTTEMENT - POINT FIXE'
        write (ifm,1070) 'ITER_FROT_MAXI  ',reacbs
        write (ifm,1071) 'RESI_FROT       ',resifr
    else if (iresof.eq.1) then
        write (ifm,*) '<CONTACT> ... ALGO. FROTTEMENT - NEWTON'
        write (ifm,1071) 'RESI_FROT       ',resifr
    else
        call assert(.false.)
    endif
!
! --- CONTACT
!
    if (iresoc .eq. 0) then
        write (ifm,*) '<CONTACT> ... ALGO. CONTACT - POINT FIXE'
        write (ifm,1070) 'ITER_CONT_MULT  ',reacca
        write (ifm,1070) 'ITER_CONT_MAXI  ',reacmx
    else if (iresoc.eq.1) then
        write (ifm,*) '<CONTACT> ... ALGO. CONTACT - NEWTON'
    else
        call assert(.false.)
    endif
!
    1070 format (' <CONTACT> ...... PARAM. : ',a16,' - VAL. : ',i5)
    1071 format (' <CONTACT> ...... PARAM. : ',a16,' - VAL. : ',e12.5)
!
    call jedema()
end subroutine
