subroutine recutb(ik1d, nomgrn, tabrev, tabmdb, tabthr)
!
    implicit none
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: ik1d
    character(len=8) :: tabrev, tabmdb, tabthr
    character(len=32) :: nomgrn
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! IN  : IK1D   : NUMERO D'OCCURENCE ------------------------------------
! OUT : NOMGRN : NOM DU GROUPE DE NOEUDS -------------------------------
! --- : TABREV : TABLE MECANIQUE DU REVETEMENT -------------------------
! --- : TABMDB : TABLE MECANIQUE DU METAL DE BASE ----------------------
! --- : TABTHR : TABLE THERMIQUE ---------------------------------------
! ======================================================================
    integer :: ibid
    character(len=8) :: motfac
! ======================================================================
    call jemarq()
! ======================================================================
! --- RECUPERATION DES TABLES ASSOCIEES A K1D POUR L'ITERATION COURANTE-
! ======================================================================
    motfac = 'K1D'
    call getvid(motfac, 'TABL_MECA_REV', iocc=ik1d, scal=tabrev, nbret=ibid)
    call getvid(motfac, 'TABL_MECA_MDB', iocc=ik1d, scal=tabmdb, nbret=ibid)
    call getvid(motfac, 'TABL_THER', iocc=ik1d, scal=tabthr, nbret=ibid)
    call getvtx(motfac, 'INTITULE', iocc=ik1d, scal=nomgrn, nbret=ibid)
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
