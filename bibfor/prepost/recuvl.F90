subroutine recuvl(nbval, tbinst, nbval2, tbinth, norev,&
                  tbscrv, nomdb, tbscmb)
!
    implicit none
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/tbexv1.h"
    integer :: nbval, nbval2, norev, nomdb
    character(len=19) :: tbinst, tbinth, tbscrv, tbscmb
! ======================================================================
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
!
! ======================================================================
! ======================================================================
! --- BUT : RECUPERATION DES TABLES MECANIQUES, THERMIQUE ET -----------
! ------- : DU GROUPE DE NOEUDS CONSIDERES -----------------------------
! ======================================================================
! OUT : NBVAL  : NOMBRE D'INSTANT DE CALCUL MECANIQUE ------------------
! --- : TBINST : VECTEUR DES INSTANTS DE CALCUL MECANIQUE --------------
! --- : NBVAL2 : NOMBRE D'INSTANT DE CALCUL THERMIQUE ------------------
! --- : TBINTH : VECTEUR DES INSTANTS DE CALCUL THERMIQUE --------------
! --- : NOREV  : NOMBRE DE NOEUDS COTE REVETEMENT ----------------------
! --- : TBSCRV : VECTEUR DES ABSCISSES CURVILIGNES COTE REVETEMENT -----
! --- : NOMDB  : NOMBRE DE NOEUDS COTE METAL DE BASE -------------------
! --- : TBSCMB : VECTEUR DES ABSCISSES CURVILIGNES COTE METAL DE BASE --
! ======================================================================
    integer :: ibid,irev
    character(len=8) :: motfac, k8b, tabrev, tabmdb, tabthr
! ======================================================================
    call jemarq()
! ======================================================================
! --- INITIALISATIONS --------------------------------------------------
! ======================================================================
    motfac = 'K1D'
! ======================================================================
! --- RECUPERATION DES TABLES ASSOCIEES A K1D POUR L'ITERATION COURANTE-
! ======================================================================
    call getvid(motfac, 'TABL_MECA_REV', iocc=1, scal=tabrev, nbret=irev)
    call getvid(motfac, 'TABL_MECA_MDB', iocc=1, scal=tabmdb, nbret=ibid)
    call getvid(motfac, 'TABL_THER', iocc=1, scal=tabthr, nbret=ibid)
    if(irev.eq.0) then
      tabrev=tabmdb
    endif
! ======================================================================
! --- RECUPERATION DES LISTES D'INSTANT --------------------------------
! ======================================================================
    call tbexv1(tabrev, 'INST', tbinst, 'V', nbval,&
                k8b)
    call tbexv1(tabthr, 'INST', tbinth, 'V', nbval2,&
                k8b)
! ======================================================================
! --- RECUPERATION DE LA LISTE DES ABSCISSES CURVILIGNES ---------------
! --- COTE REVETEMENT --------------------------------------------------
! ======================================================================
    call tbexv1(tabrev, 'ABSC_CURV', tbscrv, 'V', norev,&
                k8b)
! ======================================================================
! --- RECUPERATION DE LA LISTE DES ABSCISSES CURVILIGNES ---------------
! --- COTE METAL DE BASE -----------------------------------------------
! ======================================================================
    call tbexv1(tabmdb, 'ABSC_CURV', tbscmb, 'V', nomdb,&
                k8b)
! ======================================================================
! --- DESTRUCTION DES TABLES INUTILES ----------------------------------
! ======================================================================
    call jedetr(tabrev)
    call jedetr(tabmdb)
    call jedetr(tabthr)
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
