subroutine xrelco(noma, nliseq, lisrel, nrel)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/afrela.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    integer :: nrel
    character(len=8) :: noma
    character(len=19) :: nliseq, nlisrl, nlisco
    character(len=19) :: lisrel
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (PREPARATION - AFFE_CHAR_MECA)
!
! CREER DES RELATIONS ENTRE LES INCONNUES DE CONTACT POUR
! SATISFAIRE LA LBB CONDITION
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NLISEQ : LISTE REL. LIN. POUR V2 SEULEMENT
! IN  NBASCO : CHAM_NO POUR BASE COVARIANTE
! OUT NREL   : NOMBRE DE RELATIONS À IMPOSER
!
!
!
!
    integer :: nbddl, xxmmvd
    parameter  (nbddl=12)
    character(len=8) :: ddlc(nbddl)
!
    real(kind=8) :: rbid, betar, coefr(6)
    integer :: ier, jlis1, ndime(8), neq, i, jlis2, ibid, iret
    integer :: nuno(8), ndim, j
    character(len=8) :: noeud(8), k8bid, ddl(8)
    complex(kind=8) :: cbid
    logical :: lmulti
!
    data ddlc /'LAGS_C','LAGS_F1','LAGS_F2',&
     &            'LAG2_C','LAG2_F1','LAG2_F2',&
     &            'LAG3_C','LAG3_F1','LAG3_F2',&
     &            'LAG4_C','LAG4_F1','LAG4_F2'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INTIALISATIONS
!
    betar = 0.d0
    do 5 i = 1, 8
        ndime(i) = 0
 5  end do
!
! --- DONNÉES RELATIVES AU MAILLAGE
!
    call dismoi('F', 'DIM_GEOM', noma, 'MAILLAGE', ndim,&
                k8bid, iret)
!
! --- 1) RELATIONS D'EGALITE
!
    call jeexin(nliseq, ier)
    if (ier .eq. 0) then
        goto 100
    else
        call jeveuo(nliseq, 'L', jlis1)
        call jelira(nliseq, 'LONMAX', neq, k8bid)
        call jeexin(nliseq(1:14)//'_LAGR', ier)
        if (ier .eq. 0) then
            lmulti = .false.
        else
            lmulti = .true.
            call jeveuo(nliseq(1:14)//'_LAGR', 'L', jlis2)
        endif
    endif
!
    do 10 i = 1, neq/2
        nuno(1) = zi(jlis1-1+2*(i-1)+1)
        nuno(2) = zi(jlis1-1+2*(i-1)+2)
!
        call jenuno(jexnum(noma(1:8)//'.NOMNOE', nuno(1)), noeud(1))
        call jenuno(jexnum(noma(1:8)//'.NOMNOE', nuno(2)), noeud(2))
!
        coefr(1) = 1.d0
        coefr(2) = -1.d0
!
! --- RELATION POUR LES MULTIPLICATEURS DE CONTACT ET FROTTEMENT
!
        do 20 j = 1, ndim
            if (.not.lmulti) then
                ddl(1) = ddlc(j)
                ddl(2) = ddlc(j)
            else
                ddl(1) = ddlc(3*(zi(jlis2-1+2*(i-1)+1)-1)+j)
                ddl(2) = ddlc(3*(zi(jlis2-1+2*(i-1)+2)-1)+j)
            endif
            call afrela(coefr, cbid, ddl, noeud, ndime,&
                        rbid, 2, betar, cbid, k8bid,&
                        'REEL', 'REEL', '12', 0.d0, lisrel)
            nrel = nrel + 1
20      continue
10  end do
100  continue
!
!
    call jedema()
end subroutine
