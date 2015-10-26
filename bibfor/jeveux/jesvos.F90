subroutine jesvos(clas)
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
! ======================================================================
    implicit none
#include "jeveux_private.h"
#include "asterfort/jjlide.h"
    character(len=*) :: clas
! ----------------------------------------------------------------------
! SAUVEGARDE AVEC ECRITURE DISQUE DES OBJETS (OBJETS SIMPLES OU OBJETS 
! SYSTEME DE COLLECTION) AVANT OPERATION DE RETASSAGE
! ON POURRAIT SE LIMITER AUX OBJETS SIMPLES SYSTEME POSSEDANT $$IADD 
! DANS LEUR NOM, MAIS DE TOUTE FACON IL SERA NECESSAIRE DE TOUS LES
! DECHARGER
!
! IN  CLAS   : NOM DE LA CLASSE ASSOCIEE ( ' ' POUR TOUTES LES CLASSES )
!
!-----------------------------------------------------------------------
    integer :: i
    integer :: j, jcara, jdate, jdocu, jgenr, jhcod, jiadd
    integer :: jiadm, jlong, jlono, jltyp, jluti, jmarq, jorig
    integer :: jrnom, jtype, n, ncla1, ncla2
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &               jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
!
    integer :: nrhcod, nremax, nreuti
    common /icodje/  nrhcod(n) , nremax(n) , nreuti(n)
!
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe, nomfic(n), kstout(n), kstini(n), dn2(n)
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
! ----------------------------------------------------------------------
    character(len=1) :: kclas, clasi
    character(len=32) :: crnom
! DEB ------------------------------------------------------------------
!
    kclas = clas ( 1: min(1,len(clas)))
    if (kclas .eq. ' ') then
        ncla1 = 1
        ncla2 = index ( classe , '$' ) - 1
        if (ncla2 .lt. 0) ncla2 = n
    else
        ncla1 = index ( classe , kclas)
        ncla2 = ncla1
    endif
    do i = ncla1, ncla2
        clasi = classe(i:i)
        if (clasi .ne. ' ') then
            iclas = i
            iclaos = i
            do j = 1, nremax(i)
                idatos = j
                crnom = rnom(jrnom(i)+j)
                if (crnom(1:1) .eq. '?') goto 5
                if ( iadd(jiadd(i)+2*j-1) .eq. 0 ) then
                   call jjlide ('JETASS', crnom , 1 )
                endif
 5              continue
            end do 
        endif
    end do
! FIN ------------------------------------------------------------------
end subroutine

