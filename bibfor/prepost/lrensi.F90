subroutine lrensi(fich, long, linoch, ndim, nomo,&
                  noma, resu)
! aslint: disable=
    implicit  none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/celver.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/indiis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxlgut.h"
#include "asterfort/nbelem.h"
#include "asterfort/nbgrel.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsagsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/typele.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/ulnume.h"
#include "asterfort/ulopen.h"
#include "asterfort/wkvect.h"
    integer :: long, ndim
    character(len=8) :: resu, noma, nomo
    character(len=16) :: fich, linoch(*)
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
!
!     BUT:
!       LECTURE DES RESULTATS PRESENTS DANS LES FICHIERS ENSIGHT
!       ET STOCKAGE DANS LA SD RESULTAT
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   FICH     : NOM DU FICHIER ENSIGHT
! IN   LONG     : LONGUEUR DU NOM DU FICHIER ENSIGHT
! IN   LINOCH   : LISTE DES NOMS DE CHAMPS A LIRE
!                 ICI IL N'Y EN A QU'UN ET C'EST 'PRES'
! IN   NDIM     : DIMENSION DE LA GEOMETRIE
! IN   NOMO     : NOM DU MODELE
! IN   NOMA     : NOM DU MAILLAGE
! IN   RESU     : NOM DE LA SD_RESULTAT
!
! ......................................................................
!
!
!
!
!
    character(len=6) :: nompro
    parameter (nompro='LRENSI')
!
    integer :: iu99, iu98, iu97
    integer :: vali(2)
    integer :: npas
    integer :: iret, ll
    integer :: i, io, nscal, nvect, nflag
    integer :: ipas, i1, i2, nbno, inopr, ncarlu
    integer :: i21, nlig, ipres, nbgr, nfacha
    integer :: idec, jceld, nbelgr, iel, ima, liel
    integer :: ino, nno, ii, iadno, iad, jcelv
    integer :: ibid, nstar, j, irest
    integer :: te, igr
    integer :: jinst, itps
!
    character(len=8) :: k8b, chaine
    character(len=8) :: lpain(1), lpaout(1)
    character(len=16) :: dir, nomte
    character(len=19) :: nomch, ligrmo, chpres
    character(len=24) :: lchin(1), lchout(1)
    character(len=24) :: valk(2)
    character(len=24) :: noliel
    character(len=80) :: k80b, fires, fipres, figeom, fic80b
    character(len=24) :: chgeom, option, connex
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!            1234567890123456
    dir = 'DONNEES_ENSIGHT/'
    fic80b = fich
    fires = dir//fich(1:long)
    iu99 = ulnume ()
    call ulopen(iu99, fires, ' ', 'OLD', 'O')
!
!  LECTURE DU FICHIER RESULTS (PAS DE TEMPS
!                              ET FICHIERS GEOM ET PRES)
    read (iu99,'(3I8)',err=50,end=50,iostat=io) nscal,nvect,nflag
    read (iu99,'(1I8)',err=50,end=50,iostat=io) npas
!
    call wkvect('&&'//nompro//'.INST', 'V V R', npas, ipas)
    read (iu99,'(6(E12.5))',err=50,end=50,&
     &    iostat=io) (zr(ipas-1+i),i=1,npas)
    i1 = 0
    i2 = 0
    if (nflag .gt. 0) then
        read (iu99,'(2I8)',err=50,end=50,iostat=io) i1,i2
    else
        if (npas .ne. 1) then
            ASSERT(.false.)
        endif
    endif
    read (iu99,'(A80)',err=50,end=50,iostat=io) figeom
    long = lxlgut(figeom)
    figeom(1:16+long) = dir//figeom(1:long)
    read (iu99,'(A80)',err=50,end=50,iostat=io) fipres
    call ulopen(-iu99, ' ', ' ', ' ', ' ')
    long = lxlgut(figeom)
    fipres(1:16+long) = dir//fipres(1:long)
!
    fic80b = figeom
    iu98 = ulnume ()
    call ulopen(iu98, figeom, ' ', 'OLD', 'O')
!  LECTURE DU FICHIER GEOM (NUMEROS DE NOEUDS)
    read (iu98,'(A80)',err=50,end=50,iostat=io) k80b
    read (iu98,'(A80)',err=50,end=50,iostat=io) k80b
    read (iu98,'(1I8)',err=50,end=50,iostat=io) nbno
    call wkvect('&&'//nompro//'.NUMNOEU', 'V V I', nbno, inopr)
    do 10 i = 1, nbno
        read (iu98,'(1I8)',err=50,iostat=io) zi(inopr-1+i)
10  end do
    call ulopen(-iu98, ' ', ' ', ' ', ' ')
!
    nstar = 0
    ncarlu = 0
    ll = len(fipres)
    do 20 i = 1, ll
        if (fipres(i:i) .eq. '*') then
            nstar = nstar + 1
        endif
        if (nstar .gt. 0 .and. fipres(i:i) .eq. ' ') goto 30
        ncarlu = ncarlu + 1
20  end do
30  continue
    ll = ncarlu
    if (nstar .ge. 8) then
        vali (1) = nstar
        call u2mesg('F', 'UTILITAI8_25', 0, ' ', 1,&
                    vali, 0, 0.d0)
    endif
!
    chgeom = noma//'.COORDO'
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
!
    lpaout(1) = 'PPRES_R'
    chpres = '&&CHPRES'
    lchout(1) = chpres
    ligrmo = nomo//'.MODELE'
    option = 'TOU_INI_ELNO'
    call calcul('S', option, ligrmo, 1, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
!
!     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
    call celver(chpres, 'NBVARI_CST', 'STOP', ibid)
    call celver(chpres, 'NBSPT_1', 'STOP', ibid)
!
    call jeveuo(chpres//'.CELD', 'L', jceld)
    call jeveuo(chpres//'.CELV', 'E', jcelv)
!
!  BOUCLE SUR LES PAS DE TEMPS ET LECTURE DES PRESSIONS
!  ----------------------------------------------------
!
    do 40 itps = 1, npas
!
        i21 = i1 + (itps-1)*i2
        call codent(i21, 'D0', chaine)
        if (nstar .gt. 0) then
            fipres = fipres(1:ll-nstar)//chaine(9-nstar:8)
        endif
        fic80b = fipres
        iu97 = ulnume ()
        call ulopen(iu97, fipres, ' ', 'OLD', 'O')
        read (iu97,'(A80)',err=50,end=50,iostat=io) k80b
        call wkvect('&&'//nompro//'.PRES.'//chaine, 'V V R', nbno, ipres)
        nlig = nbno/6
        do 410 i = 1, nlig
            read (iu97,'(6(E12.5))') (zr(ipres-1+6* (i-1)+j),j=1,6)
410      continue
        irest = nbno - 6*nlig
        if (irest .gt. 0) then
            read (iu97,'(6(E12.5))',err=50,end=50, iostat=io) (zr(&
            ipres-1+6*nlig+j),j=1,irest)
        endif
        call ulopen(-iu97, ' ', ' ', ' ', ' ')
!
!  REMPLISSAGE DU .VALE DU CHAM_ELEM DE PRES_R
!
        connex = noma//'.CONNEX'
        noliel = ligrmo//'.LIEL'
        nbgr = nbgrel(ligrmo)
!
        if (ndim .ge. 3) then
            nfacha = 0
            do 411 igr = 1, nbgr
                idec = zi(jceld-1+zi(jceld-1+4+igr)+8)
                te = typele(ligrmo,igr)
                call jenuno(jexnum('&CATA.TE.NOMTE', te), nomte)
!
                if (nomte .eq. 'MECA_FACE3' .or. nomte .eq. 'MECA_FACE4' .or. nomte .eq.&
                    'MECA_FACE6' .or. nomte .eq. 'MECA_FACE8' .or. nomte .eq. 'MECA_FACE9'&
                    .or. nomte .eq. 'MEQ4QU4' .or. nomte .eq. 'MEDSQU4' .or. nomte .eq.&
                    'MEDSTR3') then
                    nbelgr = nbelem(ligrmo,igr)
                    call jeveuo(jexnum(noliel, igr), 'L', liel)
                    do 412 iel = 1, nbelgr
                        ima = zi(liel-1+iel)
                        call jeveuo(jexnum(connex, ima), 'L', iadno)
                        call jelira(jexnum(connex, ima), 'LONMAX', nno)
                        do 413 ino = 1, nno
                            ii = indiis(zi(inopr),zi(iadno-1+ino),1, nbno)
                            if (ii .eq. 0) goto 415
413                      continue
!
!   LA MAILLE IMA EST CHARGEE EN PRESSION
!
                        nfacha = nfacha + 1
                        iad = jcelv - 1 + idec - 1 + nno* (iel-1)
                        do 414 i = 1, nno
                            ii = indiis(zi(inopr),zi(iadno-1+i),1, nbno)
                            zr(iad+i) = zr(ipres-1+ii)
414                      continue
                        goto 412
415                      continue
!
!   LA MAILLE IMA N'EST PAS CHARGEE EN PRESSION
!
                        iad = jcelv - 1 + idec - 1 + nno* (iel-1)
                        do 416 i = 1, nno
                            zr(iad+i) = 0.0d0
416                      continue
!
412                  continue
                else
                    call u2mesk('A', 'UTILITAI2_91', 1, nomte)
                endif
411          continue
        endif
!
        if (ndim .eq. 2 .or. (ndim.ge.3.and.nfacha.eq.0)) then
            do 417 igr = 1, nbgr
                idec = zi(jceld-1+zi(jceld-1+4+igr)+8)
                te = typele(ligrmo,igr)
                call jenuno(jexnum('&CATA.TE.NOMTE', te), nomte)
!
                if (nomte .eq. 'MEPLSE2' .or. nomte .eq. 'MEAXSE2' .or. nomte .eq.&
                    'MEPLSE3' .or. nomte .eq. 'MEAXSE3') then
                    nbelgr = nbelem(ligrmo,igr)
                    call jeveuo(jexnum(noliel, igr), 'L', liel)
                    do 418 iel = 1, nbelgr
                        ima = zi(liel-1+iel)
                        call jeveuo(jexnum(connex, ima), 'L', iadno)
                        call jelira(jexnum(connex, ima), 'LONMAX', nno)
                        do 419 ino = 1, nno
                            ii = indiis(zi(inopr),zi(iadno-1+ino),1, nbno)
                            if (ii .eq. 0) goto 421
419                      continue
!
!   LA MAILLE IMA EST CHARGEE EN PRESSION
!
                        iad = jcelv - 1 + idec - 1 + 2*nno* (iel-1)
                        do 420 i = 1, nno
                            ii = indiis(zi(inopr),zi(iadno-1+i),1, nbno)
                            zr(iad+2*i-1) = zr(ipres-1+ii)
                            zr(iad+2*i) = 0.0d0
420                      continue
                        goto 418
421                      continue
!
!   LA MAILLE IMA N'EST PAS CHARGEE EN PRESSION
!
                        iad = jcelv - 1 + idec - 1 + 2*nno* (iel-1)
                        do 422 i = 1, 2*nno
                            zr(iad+i) = 0.0d0
422                      continue
!
418                  continue
                endif
417          continue
!
        endif
!
        call rsexch(' ', resu, linoch(1), itps, nomch,&
                    iret)
        if (iret .eq. 100) then
        else if (iret.eq.110) then
            call rsagsd(resu, 0)
            call rsexch(' ', resu, linoch(1), itps, nomch,&
                        iret)
        else
            valk (1) = resu
            valk (2) = nomch
            vali (1) = itps
            vali (2) = iret
            call u2mesg('F', 'UTILITAI8_26', 2, valk, 2,&
                        vali, 0, 0.d0)
        endif
        call copisd('CHAMP_GD', 'G', chpres, nomch)
        call rsnoch(resu, linoch(1), itps)
        call rsadpa(resu, 'E', 1, 'INST', itps,&
                    0, jinst, k8b)
        zr(jinst) = zr(ipas-1+itps)
!
        call jedetr('&&'//nompro//'.PRES.'//chaine)
!
40  end do
!
    goto 60
!
50  continue
!
!     -- MESSAGE D'ERREUR DE LECTURE :
!     --------------------------------------------
    if (io .lt. 0) then
        call u2mesg('F+', 'UTILITAI8_28', 0, ' ', 0,&
                    0, 0, 0.d0)
    else if (io.gt.0) then
        call u2mesg('F+', 'UTILITAI8_29', 0, ' ', 0,&
                    0, 0, 0.d0)
    endif
    valk (1) = fic80b(1:24)
    call u2mesg('F', 'UTILITAI8_30', 1, valk, 0,&
                0, 0, 0.d0)
!
60  continue
!
    call jedema()
!
end subroutine
