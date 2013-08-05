function cfdisi(deficz, questz)
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
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: cfdisi
    character(len=*) :: deficz
    character(len=*) :: questz
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES)
!
! RETOURNE DES INFOS DIVERSES POUR LE CONTACT (ENTIER)
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO  : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
! IN  QUESTI  : QUESTION (PARAMETRE INTERROGE)
!
! ----------------------------------------------------------------------
!
    character(len=24) :: ndimco
    integer :: jdim
    character(len=24) :: defico, questi
    character(len=24) :: paraci
    integer :: jparci
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = deficz
    questi = questz
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    paraci = defico(1:16)//'.PARACI'
    ndimco = defico(1:16)//'.NDIMCO'
!
    if (questi .eq. 'NB_ITER_GEOM') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+1-1)
!
    else if (questi.eq.'STOP_SINGULIER') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+2-1)
!
    else if (questi.eq.'NB_RESOL') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+3-1)
!
    else if (questi.eq.'FORMULATION') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+4-1)
!
    else if (questi.eq.'ITER_CONT_MULT') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+5-1)
!
    else if (questi.eq.'ITER_GEOM_MAXI') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+6-1)
!
    else if (questi.eq.'ITER_FROT_MAXI') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+7-1)
!
    else if (questi.eq.'ALL_VERIF') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+8-1)
!
    else if (questi.eq.'ITER_CONT_MAXI') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+10-1)
!
    else if (questi.eq.'ALL_INTERPENETRE') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+11-1)
!
    else if (questi.eq.'ITER_GCP_MAXI') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+12-1)
!
    else if (questi.eq.'PRE_COND') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+13-1)
!
    else if (questi.eq.'ITER_PRE_MAXI') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+14-1)
!
    else if (questi.eq.'RECH_LINEAIRE') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+15-1)
!
    else if (questi.eq.'AXISYMETRIQUE') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+16-1)
!
    else if (questi.eq.'ALGO_CONT') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+17-1)
!
    else if (questi.eq.'ALGO_FROT') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+18-1)
!
    else if (questi.eq.'LISSAGE') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+19-1)
!
    else if (questi.eq.'EXIS_XFEM_CZM') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+21-1)
!
    else if (questi.eq.'EXIS_PENA') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+22-1)
!
    else if (questi.eq.'EXIS_VERIF') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+23-1)
!
    else if (questi.eq.'ALL_INTEG_NOEUD') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+24-1)
!
    else if (questi.eq.'STOP_INTERP') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+25-1)
!
    else if (questi.eq.'EXIS_GLISSIERE') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+26-1)
!
    else if (questi.eq.'ALGO_RESO_CONT') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+27-1)
!
    else if (questi.eq.'ALGO_RESO_FROT') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+28-1)
!
    else if (questi.eq.'ALGO_RESO_GEOM') then
        call jeveuo(paraci, 'L', jparci)
        cfdisi = zi(jparci+9-1)
!
    else if (questi .eq.'CONT_XFEM_GG') then
        call jeveuo(paraci, 'L', jparci)
        if ((zi(jparci+17-1).eq.7) .and. (zi(jparci+1-1).ne.0)) then
            cfdisi = 1
        else
            cfdisi = 0
        endif
!
    else if (questi.eq.'PROJ_NEWT_ITER') then
        cfdisi = 200
!
    else if (questi.eq.'FLIP_FLOP_IMAX') then
        cfdisi = 20
!
    else if (questi.eq.'NDIM') then
        call jeveuo(ndimco, 'L', jdim)
        cfdisi = zi(jdim+1-1)
!
    else if (questi.eq.'NZOCO') then
        call jeveuo(ndimco, 'L', jdim)
        cfdisi = zi(jdim+2-1)
!
    else if (questi.eq.'NSUCO') then
        call jeveuo(ndimco, 'L', jdim)
        cfdisi = zi(jdim+3-1)
!
    else if (questi.eq.'NMACO') then
        call jeveuo(ndimco, 'L', jdim)
        cfdisi = zi(jdim+4-1)
!
    else if (questi.eq.'NNOCO') then
        call jeveuo(ndimco, 'L', jdim)
        cfdisi = zi(jdim+5-1)
!
    else if (questi.eq.'NTNOE') then
        call jeveuo(ndimco, 'L', jdim)
        cfdisi = zi(jdim+8 -1)
!
    else if (questi.eq.'NTMAE') then
        call jeveuo(ndimco, 'L', jdim)
        cfdisi = zi(jdim+9 -1)
!
    else if (questi.eq.'NTNOM') then
        call jeveuo(ndimco, 'L', jdim)
        cfdisi = zi(jdim+10-1)
!
    else if (questi.eq.'NTMAM') then
        call jeveuo(ndimco, 'L', jdim)
        cfdisi = zi(jdim+11-1)
!
    else if (questi.eq.'NTNOEC') then
        call jeveuo(ndimco, 'L', jdim)
        cfdisi = zi(jdim+12-1)
!
    else if (questi.eq.'NTMAEC') then
        call jeveuo(ndimco, 'L', jdim)
        cfdisi = zi(jdim+13-1)
!
    else if (questi.eq.'NTNOMC') then
        call jeveuo(ndimco, 'L', jdim)
        cfdisi = zi(jdim+14-1)
!
    else if (questi.eq.'NTMAMC') then
        call jeveuo(ndimco, 'L', jdim)
        cfdisi = zi(jdim+15-1)
!
    else if (questi.eq.'NTPT') then
        call jeveuo(ndimco, 'L', jdim)
        cfdisi = zi(jdim+16-1)
!
    else if (questi.eq.'NTPC') then
        call jeveuo(ndimco, 'L', jdim)
        cfdisi = zi(jdim+17-1)
!
    else if (questi.eq.'NTMANO') then
        call jeveuo(ndimco, 'L', jdim)
        cfdisi = zi(jdim+18-1)
!
    else
        write(6,*) 'QUESTION: ',questi
        ASSERT(.false.)
    endif
!
    call jedema()
!
end function
