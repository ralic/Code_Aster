subroutine op0161()
    implicit none
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     COMBINAISON FOURIER
!     ------------------------------------------------------------------
    include 'jeveux.h'
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterc/r8dgrd.h'
    include 'asterfort/assert.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/refode.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rsagsd.h'
    include 'asterfort/rscrsd.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsnoch.h'
    include 'asterfort/rsorac.h'
    include 'asterfort/wkvect.h'
    complex(kind=8) :: cbid
    character(len=8) :: k8b, resu, resuin, modele, mate, carele
    character(len=16) :: concep, nomcmd, nsymb
    character(len=24) :: nomch
    integer :: iarg
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ian, ibid, ich, ior, iordr, iret, jangl
    integer :: jcara, jcham, jcoe, jjan, jmat, jmod, jnch
    integer :: jnha, jnmo, jordr, jpara, jtch, jtmo, k
    integer :: n1, n2, n3, nbangl, nbcham, nbordr, nbtrou
!
    real(kind=8) :: angle, rbid
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
    call getres(resu, concep, nomcmd)
!
    call getvid(' ', 'RESULTAT', 1, iarg, 1,&
                resuin, n1)
    call rsorac(resuin, 'LONUTI', ibid, rbid, k8b,&
                cbid, rbid, k8b, nbordr, 1,&
                nbtrou)
    call wkvect('&&OP0161.NUME_ORDRE', 'V V I', nbordr, jordr)
    call rsorac(resuin, 'TOUT_ORDRE', ibid, rbid, k8b,&
                cbid, rbid, k8b, zi(jordr), nbordr,&
                ibid)
!
    call getvtx(' ', 'NOM_CHAM', 1, iarg, 0,&
                k8b, n2)
    nbcham = -n2
    call wkvect('&&OP0161.CHAMP', 'V V K16', nbcham, jcham)
    call getvtx(' ', 'NOM_CHAM', 1, iarg, nbcham,&
                zk16(jcham), n2)
!
    call getvr8(' ', 'ANGLE', 1, iarg, 0,&
                angle, n3)
    nbangl = -n3
    call wkvect('&&OP0161.ANGLE', 'V V R', nbangl, jangl)
    call getvr8(' ', 'ANGLE', 1, iarg, nbangl,&
                zr(jangl), n3)
!
    call rscrsd('G', resu, concep, nbangl)
!
    call wkvect('&&OP0161.NOM_CHAMP', 'V V K24', nbordr, jnch)
    call wkvect('&&OP0161.TYP_CHAMP', 'V V K8', nbordr, jtch)
    call wkvect('&&OP0161.NUM_HARMO', 'V V I', nbordr, jnha)
    call wkvect('&&OP0161.COEFFICIE', 'V V R', nbordr, jcoe)
!
    do 100 ich = 1, nbcham
        nsymb = zk16(jcham+ich-1)
        k = 0
        do 120 ior = 0, nbordr-1
            iordr = zi(jordr+ior)
            call rsexch(' ', resuin, nsymb, iordr, nomch,&
                        iret)
            if (iret .eq. 0) then
                k = k + 1
                zk24(jnch+k-1) = nomch
                call rsadpa(resuin, 'L', 1, 'TYPE_MODE', iordr,&
                            0, jtmo, k8b)
                zk8(jtch+k-1) = zk8(jtmo)
                call rsadpa(resuin, 'L', 1, 'NUME_MODE', iordr,&
                            0, jnmo, k8b)
                zi(jnha+k-1) = zi(jnmo)
                zr(jcoe+k-1) = 1.d0
                call rsadpa(resuin, 'L', 1, 'MODELE', iordr,&
                            0, jmod, k8b)
                modele = zk8(jmod)
                call rsadpa(resuin, 'L', 1, 'CHAMPMAT', iordr,&
                            0, jmat, k8b)
                mate = zk8(jmat)
                call rsadpa(resuin, 'L', 1, 'CARAELEM', iordr,&
                            0, jcara, k8b)
                carele = zk8(jcara)
            endif
120      continue
!
        do 130 ian = 1, nbangl
!
!     STOCKAGE DU NOM DU MODELE
!     -------------------------
            call rsadpa(resu, 'E', 1, 'MODELE', ian,&
                        0, jpara, k8b)
            zk8(jpara)=modele
!
!     STOCKAGE DU NOM DU CHAMP MATERIAU
!     ---------------------------------
            call rsadpa(resu, 'E', 1, 'CHAMPMAT', ian,&
                        0, jpara, k8b)
            zk8(jpara)=mate
!
!     STOCKAGE DU NOM DE LA CARACTERISTIQUE ELEMENTAIRE
!     -------------------------------------------------
            call rsadpa(resu, 'E', 1, 'CARAELEM', ian,&
                        0, jpara, k8b)
            zk8(jpara)=carele
!
130      continue
!
        if (k .ne. 0) then
            do 110 ian = 1, nbangl
                call rsexch(' ', resu, nsymb, ian, nomch,&
                            iret)
                if (iret .eq. 110) then
                    call rsagsd(resu, 0)
                    call rsexch(' ', resu, nsymb, ian, nomch,&
                                iret)
                else if (iret .eq. 100) then
                else
                    call assert(.false.)
                endif
                angle = zr(jangl+ian-1) * r8dgrd()
                call refode(k, angle, zk24(jnch), zi(jnha), zk8( jtch),&
                            zr(jcoe), 'G', nomch)
                call rsnoch(resu, nsymb, ian)
                call rsadpa(resu, 'E', 1, 'ANGLE', ian,&
                            0, jjan, k8b)
                zr(jjan) = zr(jangl+ian-1)
110          continue
        endif
100  end do
!
    call jedema()
end subroutine
