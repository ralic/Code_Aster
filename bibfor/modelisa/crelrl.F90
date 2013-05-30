subroutine crelrl(typcoz, typvaz, basez, lisrez)
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
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    character(len=1) :: base
    character(len=4) :: typcoe, typval
    character(len=19) :: lisrel
    character(len=*) :: typcoz, typvaz, basez, lisrez
! ------------------------------------------------------------
!     CREATION DE L'OBJET DE TYPE LISTE_DE_RELATIONS
!     DE NOM LISREL
!        LE NOM LISREL EST FOURNI EN ARGUMENT
!     SI L'OBJET LISREL EXISTE DEJA, ON LE DETRUIT
!                               PUIS ON LE RECREE
!
!     LA TAILLE DES VECTEURS CONTENANT LES COMPOSANTES
!     DES RELATIONS EST DIMENSIONNEE A LVECRL = 10000
!
!     LA TAILLE DES VECTEURS QUI CORRESPOND AUX NOMBRES
!     DE RELATIONS EST DIMENSIONNEE A NBRELA = 1000
!
!
!-------------------------------------------------------------
! TYPCOZ        - IN - K4  - : TYPE DES COEFFICIENTS DE LA RELATION
!               -    -     -   = 'REEL' OU 'COMP'
!-------------------------------------------------------------
!  TYPVAZ       - IN - K4  - : INDICATEUR DU TYPE DU SECOND MEMBRE
!               -    -     -     = 'REEL' OU 'COMP' OU 'FONC'
!-------------------------------------------------------------
!  BASEZ        - IN - K1  - : INDICATEUR DE LA BASE SUR LAQUELLE
!                              DOIT ETRE CREE L'OBJET DE TYPE
!                              LISTE_DE_RELATIONS
!-------------------------------------------------------------
!  LISREZ     - IN    - K19 - : NOM DE LA LISTE_RELA
!             - JXOUT -     -
!-------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
    integer :: idbeta, idcoef, iddl, idlagr, idnbre, idnoeu, idpoin
    integer :: idsurc, idterm, idtyco, idtyva, iret, lvecrl, nbrela
!
!-----------------------------------------------------------------------
    parameter (lvecrl = 10000)
    parameter (nbrela = 1000)
!
    call jemarq()
!
! --- SI L'OBJET LISREL EXISTE , ON LE DETRUIT ---
!
    typcoe = typcoz
    typval = typvaz
    base = basez
    lisrel = lisrez
!
    call jeexin(lisrel//'.RLCO', iret)
    if (iret .ne. 0) then
        call jedetr(lisrel//'.RLCO')
        call jedetr(lisrel//'.RLDD')
        call jedetr(lisrel//'.RLNO')
        call jedetr(lisrel//'.RLBE')
        call jedetr(lisrel//'.RLNT')
        call jedetr(lisrel//'.RLPO')
        call jedetr(lisrel//'.RLNR')
        call jedetr(lisrel//'.RLSU')
        call jedetr(lisrel//'.RLTC')
        call jedetr(lisrel//'.RLTV')
        call jedetr(lisrel//'.RLBE')
        call jedetr(lisrel//'.RLLA')
    endif
!
! ---  VECTEUR DES COEFFICIENTS DES TERMES DES RELATIONS
!
    if (typcoe .eq. 'COMP') then
        call wkvect(lisrel//'.RLCO', base//' V C', lvecrl, idcoef)
    else
        call wkvect(lisrel//'.RLCO', base//' V R', lvecrl, idcoef)
    endif
!
! ---  VECTEUR DES NOMS DES DDLS IMPLIQUES DANS LES RELATIONS
!
    call wkvect(lisrel//'.RLDD', base//' V K8', lvecrl, iddl)
!
! ---  VECTEUR DES NOMS DES NOEUDS IMPLIQUES DANS LES RELATIONS
!
    call wkvect(lisrel//'.RLNO', base//' V K8', lvecrl, idnoeu)
!
! ---  VECTEUR DES VALEURS DES SECONDS MEMBRES DE CHAQUE RELATION
!
    if (typval .eq. 'REEL') then
        call wkvect(lisrel//'.RLBE', base//' V R', nbrela, idbeta)
    else if (typval.eq.'COMP') then
        call wkvect(lisrel//'.RLBE', base//' V C', nbrela, idbeta)
    else if (typval.eq.'FONC') then
        call wkvect(lisrel//'.RLBE', base//' V K24', nbrela, idbeta)
    endif
!
! ---  VECTEUR DES NOMBRES DE TERMES DE CHAQUE RELATION
!
    call wkvect(lisrel//'.RLNT', base//' V I', nbrela, idterm)
!
! ---  VECTEUR DES NOMBRES DE TERMES CUMULES DES RELATIONS
! ---  ( SERT DE POINTEUR DANS LES TABLEAUX RELATIFS AUX TERMES
! ---    DES RELATIONS)
!
    call wkvect(lisrel//'.RLPO', base//' V I', nbrela, idpoin)
!
! ---  VECTEUR D'INDICATEURS DE PRISE EN COMPTE DES RELATIONS
! ---  ( SERT A APPLIQUER LA REGLE DE SURCHARGE)
!
    call wkvect(lisrel//'.RLSU', base//' V I', nbrela, idsurc)
!
! ---  VECTEUR DE K8 INDIQUANT LA POSITION DES LAGRANGE
! ---  ASSOCIES A LA RELATION COURANTE
!
    call wkvect(lisrel//'.RLLA', base//' V K8', nbrela, idlagr)
!
! ---  NOMBRE DE RELATIONS
!
    call wkvect(lisrel//'.RLNR', base//' V I', 1, idnbre)
    zi(idnbre) = 0
!
! ---  TYPE DES COEFFICIENTS DES RELATIONS (TYPCOE)
!
    call wkvect(lisrel//'.RLTC', base//' V K8', 1, idtyco)
    zk8(idtyco) = typcoe(1:4)//'    '
!
! ---  TYPE DES VALEURS DES RELATIONS (TYPVAL)
!
    call wkvect(lisrel//'.RLTV', base//' V K8', 1, idtyva)
    zk8(idtyva) = typval(1:4)//'    '
!
    call jedema()
end subroutine
