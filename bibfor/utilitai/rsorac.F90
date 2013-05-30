subroutine rsorac(nomsd, acces, ival, rval, kval,&
                  cval, epsi, crit, nutrou, ndim,&
                  nbtrou)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/codent.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/lxliis.h'
    include 'asterfort/rsindi.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    integer :: nbtrou, nutrou(*), ival, ndim
    real(kind=8) :: rval, epsi
    character(len=*) :: nomsd, acces, kval, crit
    complex(kind=8) :: cval
! ----------------------------------------------------------------------
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
!      RECUPERATION DU NUMERO D'ORDRE
!      D'UNE STRUCTURE DE DONNEES "SD_RESULTAT".
!      A PARTIR D'UNE VARIABLE D'ACCES.
!      ( CETTE ROUTINE FONCTIONNE AUSSI AVEC UN CONCEPT CHAMP_GD SI
!        RVAL,IVAL,..= 0 OU ACCES='DERNIER' ALORS NUTROU=1 )
! ----------------------------------------------------------------------
! IN  : NOMSD  : NOM DE LA SD "SD_RESULTAT".
!
! IN  : ACCES  : NOM SYMBOLIQUE DE LA VARIABLE D'ACCES.
!      OU BIEN : 'LONUTI','LONMAX','PREMIER','DERNIER','TOUT_ORDRE'
! ATTENTION : ACCES='LONUTI' OU 'LONMAX' NE RENVOIENT PAS UNE LISTE
!    DE NUMEROS D'ORDRE MAIS LEUR NOMBRE.
!    NBTROU=1 ET NUTROU= NOMBRE TROUVE !!
!
! IN  : IVAL   : VALEUR DE LA VARIABLE D'ACCES (SI ENTIER).
! IN  : RVAL   : VALEUR DE LA VARIABLE D'ACCES (SI REEL).
! IN  : CVAL   : VALEUR DE LA VARIABLE D'ACCES (SI COMPLEXE).
! IN  : EPSI   : PRECISION DE LA VARIABLE D'ACCES (RELATIVE/ABSOLUE).
! IN  : CRIT   : CRITERE DE PRECISION : 'RELATIF' OU 'ABSOLU'
!                (UNE VARIABLE D'ACCES EST DECLAREE VALIDE SI ELLE
!                                  SATISFAIT LE TEST RELATIF OU ABSOLU)
! IN  : NDIM   : DIMENSION DE LA LISTE NUTROU.
! OUT : NUTROU : LISTE DES NUMEROS D'ORDRE TROUVES.
! OUT : NBTROU : NOMBRE DE NUMEROS D'ORDRE TROUVES.
!              (SI LE NOMBRE TROUVE EST > NDIM, ON REND NBTROU=-NBTROU)
! ----------------------------------------------------------------------
    character(len=4) :: tysd, type, tysca
    character(len=8) :: nomobj, k8bid, k8debu, k8maxi, k8ent
    character(len=16) :: acce2
    character(len=19) :: noms2
! ----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iacces, iaobj, iatava, ibid, idebu, ier1
    integer :: ier2, iloty, imaxi, jordr, nbordr, nordr, numed
!
!-----------------------------------------------------------------------
    call jemarq()
    noms2 = nomsd
    acce2 = acces
!
!
!     --- CONCEPT CHAMP-GD
!     ----------------------------
    call jelira(noms2//'.DESC', 'DOCU', ibid, tysd)
    if ((tysd.eq.'CHNO') .or. (tysd.eq.'CHML') .or. (tysd.eq.'CART')) then
        if ((acce2.eq.'LONUTI') .or. (ival.eq.0) .or. (rval .eq. 0.d0) .or.&
            (cval.eq. (0.d0,0.d0))) then
            if (ndim .gt. 0) then
                nbtrou = 1
                nutrou(1) = 1
            else
                nbtrou = -1
            endif
        else
            call u2mess('F', 'UTILITAI4_46')
        endif
        goto 20
    endif
!
!
!     --- CONCEPT RESULTAT
!     ----------------------------
    if (acce2 .eq. 'LONUTI') then
        if (ndim .gt. 0) then
            nbtrou = 1
            call jelira(noms2//'.ORDR', 'LONUTI', nutrou(1), k8bid)
        else
            nbtrou = -1
        endif
        goto 20
!
    else if (acce2.eq.'LONMAX') then
        if (ndim .gt. 0) then
            nbtrou = 1
            call jelira(noms2//'.ORDR', 'LONMAX', nutrou(1), k8bid)
        else
            nbtrou = -1
        endif
        goto 20
!
    else if (acce2.eq.'DERNIER') then
        if (ndim .gt. 0) then
            nbtrou = 1
            call jelira(noms2//'.ORDR', 'LONUTI', numed, k8bid)
            call jeveuo(noms2//'.ORDR', 'L', jordr)
            nutrou(1) = zi(jordr+numed-1)
        else
            nbtrou = -1
        endif
        goto 20
!
    else if (acce2.eq.'PREMIER') then
        if (ndim .gt. 0) then
            nbtrou = 1
            call jeveuo(noms2//'.ORDR', 'L', jordr)
            nutrou(1) = zi(jordr-1+1)
        else
            nbtrou = -1
        endif
        goto 20
!
    else if (acce2.eq.'TOUT_ORDRE') then
        call jelira(noms2//'.ORDR', 'LONUTI', nordr, k8bid)
        if (nordr .le. ndim) then
            nbtrou = nordr
            call jeveuo(noms2//'.ORDR', 'L', jordr)
            do 10 i = 1, nordr
                nutrou(i) = zi(jordr+i-1)
10          continue
        else
            nbtrou = -nordr
            call jeveuo(noms2//'.ORDR', 'L', jordr)
            do 11 i = 1, ndim
                nutrou(i) = zi(jordr+i-1)
11          continue
        endif
        goto 20
!
    endif
!
    call jenonu(jexnom(noms2//'.NOVA', acce2), iacces)
    if (iacces .eq. 0) call u2mesk('F', 'UTILITAI4_47', 1, acce2)
!
    call jeveuo(jexnum(noms2//'.TAVA', iacces), 'L', iatava)
    nomobj = zk8(iatava-1+1)
    k8maxi = zk8(iatava-1+3)
    call lxliis(k8maxi, imaxi, ier2)
    k8debu = zk8(iatava-1+2)
    call lxliis(k8debu, idebu, ier1)
    call assert(imaxi.gt.0)
    call assert((idebu.gt.0).and.(idebu.le.imaxi))
!
    call jeveuo(noms2//'.ORDR', 'L', jordr)
    call jeveuo(noms2//nomobj, 'L', iaobj)
    call jelira(noms2//nomobj, 'TYPE', ibid, type)
    call jelira(noms2//nomobj, 'LTYP', iloty, k8bid)
    call jelira(noms2//'.ORDR', 'LONUTI', nbordr, k8bid)
    call codent(iloty, 'G', k8ent)
    tysca = type(1:1)//k8ent(1:3)
!
    call rsindi(tysca, iaobj-1+idebu, imaxi, jordr, ival,&
                rval, kval, cval, epsi, crit,&
                nbordr, nbtrou, nutrou, ndim)
!
20  continue
    call jedema()
end subroutine
