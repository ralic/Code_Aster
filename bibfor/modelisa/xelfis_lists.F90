subroutine xelfis_lists(fiss, modele, elfiss_heav,&
                            elfiss_ctip, elfiss_hect)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    character(len=8) :: fiss, modele
    character(len=24) :: elfiss_heav, elfiss_ctip, elfiss_hect
!
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
!
! person_in_charge: sam.cuvilliez at edf.fr
! ----------------------------------------------------------------------
!
!  Routine utiiltaire XFEM
!  -----------------------
!
!  Extraire les listes de mailles :
!   - elfiss_heav;
!   - elfiss_ctip;
!   - elfiss_hect;
!  qui sont respectivement des sous-listes des listes de mailles :
!   - fiss '.MAILFISS.HEAV';
!   - fiss '.MAILFISS.CTIP';
!   - fiss '.MAILFISS.HECT'.
!  Dans chaque liste , les mailles retenues sont celles qui 
!  portent des elements finis dans modele.
!
!  ATTENTION : les vecteurs jeveux elfiss_heav, elfiss_ctip et elfiss_hect
!              sont alloues dans cette routine (dans la base volatile), 
!              ils doivent etre nommes dans la routine appelante puis 
!              detruits dans la routine appelante
! ----------------------------------------------------------------------
!
!  in  fiss        : nom de la sd_fiss_xfem contenant les listes a filtrer
!  in  modele      : nom de la sd_modele permettant de definir le filtre
!  out elfiss_heav : nom de vecteur jeveux, sous-liste de fiss '.MAILFISS.HEAV'
!  out elfiss_ctip : nom de vecteur jeveux, sous-liste de fiss '.MAILFISS.CTIP'
!  out elfiss_hect : nom de vecteur jeveux, sous-liste de fiss '.MAILFISS.HECT'
!
! ----------------------------------------------------------------------
!
    character(len=24) :: mafiss(3), elfiss(3)
    integer, pointer :: p_mail_affe(:) => null()
    integer :: iret, ima, i, k, cpt, nb_ma_lis, nb_el_lis
    integer :: iadr_lis_ma, iadr_lis_el
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! - recup de la liste des mailles qui portent des EF dans la sd_modele
!
    call jeveuo(modele//'.MAILLE', 'L', vi=p_mail_affe)
!
! - boucle sur les 3 types possibles de liste de mailles 
!
    mafiss(1) = fiss//'.MAILFISS.HEAV'
    mafiss(2) = fiss//'.MAILFISS.CTIP'
    mafiss(3) = fiss//'.MAILFISS.HECT'
    elfiss(1) = elfiss_heav
    elfiss(2) = elfiss_ctip
    elfiss(3) = elfiss_hect
!
    do k = 1,3
!
        nb_ma_lis   = 0
        iadr_lis_ma = 0
!
! ----- la liste existe-t-elle
!
        call jeexin(mafiss(k), iret)
        if (iret .ne. 0) then
!
! --------- recup de la liste courante
!
            call jeveuo(mafiss(k), 'L', jadr=iadr_lis_ma)
            call jelira(mafiss(k), 'LONMAX', nb_ma_lis)
!
! --------- compter dans la liste courante les mailles qui portent des EF
!
            nb_el_lis = 0
            do i = 1,nb_ma_lis
                ima = zi(iadr_lis_ma-1+i)
                if (p_mail_affe(ima) .ne. 0) nb_el_lis = nb_el_lis+1
            enddo
!
! --------- creer la sous-liste
!
            call wkvect(elfiss(k), 'V V I', nb_el_lis, jadr=iadr_lis_el)
!
! --------- remplir la sous-liste
!
            cpt = 0
            do i = 1,nb_ma_lis
                ima = zi(iadr_lis_ma-1+i)
                if (p_mail_affe(ima) .ne. 0) then
                    cpt = cpt+1
                    zi(iadr_lis_el-1+cpt) = ima
                endif
            enddo
!
        endif
!
! - fin boucle sur les 3 types possibles de liste de mailles 
!
    enddo
!
    call jedema()
!
end subroutine
