subroutine w039c2(nuzone, jvale, jdesc, nomgd, ifm,&
                  ifr)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exisdg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    integer :: nuzone, jvale, jdesc, ifm, ifr
    character(len=8) :: nomgd
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
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!     BUT: IMPRIMER LA"LEGENDE" POUR W039C1
! ----------------------------------------------------------------------
!
!
!
!
    integer :: nec, ncmpmx, kcmp, ibid, jcmp, nzonmx, dec1, ico, debgd
    character(len=8) :: kbid, tsca, nocmp
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call dismoi('F', 'NB_EC', nomgd, 'GRANDEUR', nec,&
                kbid, ibid)
    call dismoi('F', 'TYPE_SCA', nomgd, 'GRANDEUR', ibid,&
                tsca, ibid)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', ncmpmx, kbid)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', jcmp)
    call assert(nuzone.gt.0)
    nzonmx=zi(jdesc-1+2)
!
    write (ifm,1005)'VALEUR =',dble(nuzone)
    write (ifr,1005)'VALEUR =',dble(nuzone)
    debgd=3+2*nzonmx+(nuzone-1)*nec+1
    ico=0
    do 10,kcmp=1,ncmpmx
    if (exisdg(zi(jdesc-1+debgd),kcmp)) then
        ico=ico+1
        nocmp=zk8(jcmp-1+kcmp)
        dec1=ncmpmx*(nuzone-1)+ico
        if (tsca .eq. 'K8') then
            write (ifm,1004)nocmp,'=',zk8(jvale-1+dec1)
            write (ifr,1004)nocmp,'=',zk8(jvale-1+dec1)
        else if (tsca.eq.'K16') then
            write (ifm,1004)nocmp,'=',zk16(jvale-1+dec1)
            write (ifr,1004)nocmp,'=',zk16(jvale-1+dec1)
        else if (tsca.eq.'K24') then
            write (ifm,1004)nocmp,'=',zk24(jvale-1+dec1)
            write (ifr,1004)nocmp,'=',zk24(jvale-1+dec1)
        else if (tsca.eq.'I') then
            write (ifm,1003)nocmp,'=',zi(jvale-1+dec1)
            write (ifr,1003)nocmp,'=',zi(jvale-1+dec1)
        else if (tsca.eq.'R') then
            write (ifm,1001)nocmp,'=',zr(jvale-1+dec1)
            write (ifr,1001)nocmp,'=',zr(jvale-1+dec1)
        else if (tsca.eq.'C') then
            write (ifm,1002)nocmp,'=',dble(zc(jvale-1+dec1)),&
                dimag(zc(jvale-1+dec1))
            write (ifr,1002)nocmp,'=',dble(zc(jvale-1+dec1)),&
                dimag(zc(jvale-1+dec1))
        else
            call assert(.false.)
        endif
    endif
    10 end do
!
    1001 format (4x,a8,1x,a1,1x,1pd11.3)
    1002 format (4x,a8,1x,a1,1x,2(1pd11.3))
    1003 format (4x,a8,1x,a1,1x,i8)
    1004 format (4x,a8,1x,a1,1x,a)
    1005 format (a,1x,1f11.0)
    call jedema()
end subroutine
