subroutine op0110()
    implicit none
!-----------------------------------------------------------------------
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
!
!  BUT:  OPERATEUR DE CREATION D'UN MAILLAGE SQUELETTE
!-----------------------------------------------------------------------
!
!
!
!
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterfort/cargeo.h'
    include 'asterfort/cla110.h'
    include 'asterfort/cyc110.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/rec110.h'
    include 'asterfort/titre.h'
    integer :: ioc1, ioc2, ioc3, ioc11, llref, llnbs, nbsect, ioc12, ibid
    character(len=8) :: modelg, rescyc, nomres, noma, nomsqu
    character(len=16) :: nomope, nomcmd
    integer :: iarg
!-----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
    call getres(nomres, nomcmd, nomope)
    call getfac('CYCLIQUE', ioc1)
    call getvid(' ', 'MODELE_GENE', 1, iarg, 1,&
                modelg, ioc2)
    call getvid(' ', 'SQUELETTE', 1, iarg, 1,&
                nomsqu, ioc3)
!
!
!------------------------CAS CYCLIQUE-----------------------------------
!
    if (ioc1 .gt. 0) then
        call getvid('CYCLIQUE', 'MODE_CYCL', 1, iarg, 1,&
                    rescyc, ioc11)
        if (ioc11 .gt. 0) then
            call jeveuo(rescyc//'.CYCL_REFE', 'L', llref)
            noma = zk24(llref)
            call jeveuo(rescyc//'.CYCL_NBSC', 'L', llnbs)
            nbsect = zi(llnbs)
        else
            call getvid('CYCLIQUE', 'MAILLAGE', 1, iarg, 1,&
                        noma, ioc12)
            call getvis('CYCLIQUE', 'NB_SECTEUR', 1, iarg, 1,&
                        nbsect, ibid)
        endif
        call cyc110(nomres, noma, nbsect)
!
!--------------------------CAS CLASSIQUE--------------------------------
!
    else if (ioc2 .ne. 0) then
        if (ioc3 .eq. 0) then
            call cla110(nomres, modelg)
        else
!           --- FUSION DES NOEUDS D'INTERFACE D'UN SQUELETTE EXISTANT --
            call rec110(nomres, nomsqu, modelg)
!           -- L'OBJET .INV.SKELETON EST FAUX : ON LE DETRUIT
            call jedetr(nomres//'.INV.SKELETON')
        endif
    endif
!
!
! --- CARACTERISTIQUES GEOMETRIQUES :
!     -----------------------------
    call detrsd('L_TABLE', nomres)
    call cargeo(nomres)
!
    call titre()
!
    call jedema()
end subroutine
