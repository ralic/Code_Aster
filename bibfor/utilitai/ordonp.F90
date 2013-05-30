subroutine ordonp(nomfon)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jedupo.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    character(len=19) :: nomfon
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
! person_in_charge: mathieu.courtois at edf.fr
! ----------------------------------------------------------------------
!     APPELE PAR ORDONN POUR REORDONNER LES FONCTIONS D UNE NAPPE
!     PAR ORDRE CROISSANT DES PARAMETRES
    character(len=19) :: fonc0
    character(len=24) :: chval, chpar, sfval, sfpar, chbid
    integer :: ipar, lpar, nbpara, ior, ival, lval
    integer :: i, j, it, nbp, nbpt
    real(kind=8) :: xt
!     ------------------------------------------------------------------
!
    call jemarq()
!
!     OBJET INITIAL RECOPIE DANS FONC0
    fonc0='&&ORDONP.FONC      '
    chval=nomfon//'.VALE'
    chpar=nomfon//'.PARA'
    sfval=fonc0//'.VALE'
    sfpar=fonc0//'.PARA'
!
    call jelira(chpar, 'LONUTI', nbpara, chbid)
!     RECUPERE LES PARAMETRES
    call jedupo(chpar, 'V', sfpar, .false.)
    call jeveuo(sfpar, 'E', ipar)
    call jelira(sfpar, 'LONUTI', nbpara, chbid)
!
!      CALL JEDUPC('G',CHVAL,1,'V',SFVAL,.FALSE.)
    call jedupo(chval, 'V', sfval, .false.)
!
    call jedetr(chpar)
    call jedetr(chval)
!
!     TABLEAU D'ORDRE
    call wkvect(fonc0//'.ORDR', 'V V I', nbpara, ior)
    do 11 i = 1, nbpara
        zi(ior-1+i)=i
11  end do
!
!     TRI DES PARAMETRES
    do 10 i = 1, nbpara-1
        do 10 j = i+1, nbpara
            if (zr(ipar-1+i) .gt. zr(ipar-1+j)) then
                xt = zr(ipar-1+i)
                it = zi(ior-1+i)
                zr(ipar-1+i) = zr(ipar-1+j)
                zi(ior-1+i) = zi(ior-1+j)
                zr(ipar-1+j) = xt
                zi(ior-1+j) = it
            endif
10      continue
!
!     CALCULE LA TAILLE CUMULEE DE LA COLLECTION
    nbpt=0
    do 113 i = 1, nbpara
        call jelira(jexnum(sfval, i), 'LONMAX', nbp, chbid)
        nbpt=nbpt+nbp
113  end do
!
!     --- CREATION DE L'OBJET NOMFON.PARA ---
    call wkvect(chpar, 'G V R', nbpara, lpar)
!     --- CREATION DE LA COLLECTION NOMFON.VALE ---
    call jecrec(chval, 'G V R', 'NU', 'CONTIG', 'VARIABLE',&
                nbpara)
    call jeecra(chval, 'LONT', nbpt, ' ')
    do 100 i = 1, nbpara
!        REMPLISSAGE DU .PARA
        zr(lpar-1+i)=zr(ipar-1+i)
!        REMPLISSAGE DES .VALE EN FONCTION DE L'ORDRE
        call jelira(jexnum(sfval, zi(ior-1+i)), 'LONMAX', nbp, chbid)
        call jeveuo(jexnum(sfval, zi(ior-1+i)), 'E', ival)
        call jecroc(jexnum(chval, i))
        call jeecra(jexnum(chval, i), 'LONMAX', nbp, ' ')
        call jeecra(jexnum(chval, i), 'LONUTI', nbp, ' ')
        call jeveuo(jexnum(chval, i), 'E', lval)
        do 101 j = 1, nbp
            zr(lval+j-1)=zr(ival+j-1)
101      continue
100  end do
!
!     DESTRUCTION DES OBJETS DE TRAVAIL
    call jedetr(sfpar)
    call jedetr(sfval)
    call jedetr(fonc0//'.ORDR')
!
    call jedema()
end subroutine
