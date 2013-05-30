subroutine montee(opt, ligrel, nout, lchout, lpaout,&
                  fin)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!
!     ARGUMENTS:
!     ----------
    include 'asterfort/monte1.h'
    include 'asterfort/typele.h'
    integer :: opt, nout
    character(len=*) :: lchout(*), fin
    character(len=8) :: lpaout(*)
    character(len=19) :: ligrel
! ----------------------------------------------------------------------
!     ENTREES:
!     OPT    : OPTION DE CALCUL
!     IGR    : NUMERO DU GREL DONT ON SAUVE LES CHAMPS LOCAUX (COMMON)
!     FIN    : NECESSAIRE A CAUSE DE CALVOI=1
!
!     SORTIES:
!     MET A JOUR LES CHAMPS GLOBAUX DE SORTIE DE L OPTION OPT
! ----------------------------------------------------------------------
    integer :: evfini, calvoi, jrepe, jptvoi, jelvoi
    common /caii19/evfini,calvoi,jrepe,jptvoi,jelvoi
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
!
    integer :: igr2, te2
!
!
!
    if (calvoi .eq. 0) then
        if (fin .ne. 'FIN') then
            igr2=igr
            te2=typele(ligrel,igr2)
            call monte1(opt, te2, nout, lchout, lpaout,&
                        igr2)
        endif
    else
!       -- ON RECOPIE TOUT A LA FIN :
        if (fin .eq. 'FIN') then
            do 1, igr2=1,nbgr
            te2=typele(ligrel,igr2)
            call monte1(opt, te2, nout, lchout, lpaout,&
                        igr2)
 1          continue
        endif
    endif
!
!
end subroutine
