subroutine dbgobj(ojbz, perm, iunit, mess)
    implicit none
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
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!     BUT : IMPRIMER SUR LE FICHIER "IUNIT"
!           QUELQUES VALEURS QUI "RESUMENT" UN OBJET JEVEUX (OJB)
!           (SIMPLE OU COLLECTION)
!     ARGUMENTS :
!       IN  OJB    (K24) : NOM DE L'OBJET A IMPRIMER
!       IN  IUNIT  (I)   : NUMERO DE L'UNITE LOGIQUE D'IMPRESSION
!       IN  MESS   (K*)  : "MESSAGE" PREFIXANT LA LIGNE IMPRIMEE
!       IN  PERM    K3 : /OUI/NON
!           NON : ON FAIT LA SOMME BETE DES ELEMENTS DU VECTEUR
!                 => UNE PERMUTATION DU VECTEUR NE SE VOIT PAS !
!           OUI : ON FAIT UNE "SOMME" QUI DONNE UN RESULTAT
!                 DEPENDANT UN PEU DE L'ORDRE DES ELEMENTS DU VECTEUR
! ----------------------------------------------------------------------
    include 'asterfort/assert.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/tstobj.h'
    integer :: iunit, ni
    character(len=24) :: ojb
    character(len=3) :: type
    character(len=*) :: mess, ojbz, perm
    integer :: sommi, resume, lonmax, lonuti, iret
    real(kind=8) :: sommr
!
! DEB-------------------------------------------------------------------
    ojb=ojbz
    call jeexin(ojb, iret)
    if (iret .eq. 0) goto 9999
!
    call tstobj(ojb, perm, resume, sommi, sommr,&
                lonuti, lonmax, type, iret, ni)
    if (type(1:1) .eq. 'K') then
        write (iunit,1002) mess,ojb,lonmax,lonuti,type,iret,sommi
    else if (type(1:1).eq.'I') then
        write (iunit,1002) mess,ojb,lonmax,lonuti,type,iret,sommi
    else if (type(1:1).eq.'L') then
        write (iunit,1002) mess,ojb,lonmax,lonuti,type,iret,sommi
    else if (type(1:1).eq.'R') then
        write (iunit,1003) mess,ojb,lonmax,lonuti,type,iret,ni,sommr
    else if (type(1:1).eq.'C') then
        write (iunit,1003) mess,ojb,lonmax,lonuti,type,iret,ni,sommr
    else if (type(1:1).eq.'?') then
        if (iret .ne. 0) then
            write (iunit,1004) mess,ojb,type,iret
        else
            call assert(.false.)
        endif
    else
        call assert(.false.)
    endif
!
9999  continue
!
    1002 format (a,' | ',a24,' | LONMAX=',i12,' | LONUTI=',i12,&
     &        ' | TYPE=',a4,' | IRET=',i7, ' | SOMMI=',i24 )
!
    1003 format (a,' | ',a24,' | LONMAX=',i12,' | LONUTI=',i12,&
     &        ' | TYPE=',a4,' | IRET=',i7,' | IGNORE=',i7,&
     &        ' | SOMMR=',e20.11)
!
    1004 format (a,' | ',a24, ' | TYPE=',a4,' | IRET=',i7)
!
end subroutine
