subroutine orth99(nomres, ritz)
    implicit  none
    include 'jeveux.h'
!
    include 'asterc/gettco.h'
    include 'asterc/getvid.h'
    include 'asterc/getvtx.h'
    include 'asterfort/copmod.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetc.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mtdscr.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rscrsd.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsnoch.h'
    include 'asterfort/rsorac.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vecind.h'
    include 'asterfort/vpgskp.h'
    include 'asterfort/vtcrem.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: nomres
    integer :: ritz
!----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!----------------------------------------------------------------------
!
!         DEFI_BASE_MODALE : ORTHO_BASE
!
! CE MOT CLE PERMET D'ORTHONORMALISER UNE BASE MODALE QUELCONQUE
!
!----------------------------------------------------------------------
!
!
    integer :: ifm, niv, n1, ier, ibid, imatra, nbmode, jordm, iadri1, iddeeq
    integer :: llnequ, neq, idmode, jtrav1, jtrav3, jtrav4, iorol, iorne, iad
    integer :: jiad, jvale, ieq, i, nindep, lrefe
    real(kind=8) :: alpha, rbid
    complex(kind=8) :: cbid
    character(len=8) :: k8b, matras, base, ortho, intf
    character(len=16) :: typbas
    character(len=14) :: nu, numdd1, numdda, matri1
    character(len=19) :: matr, chamol
    integer :: iarg
!----------------------------------------------------------------------
    call jemarq()
!
    alpha = 0.717d0
!
!     ---RECUPERATION DU NIVEAU D'IMPRESSION---
!
    call infmaj()
    call infniv(ifm, niv)
!
!----------------------------------------------------------------------
! --- RECUPERATION DE LA MATRICE ASSEMBLEE
!-----------------------------------------------------------------------
!
    if (ritz .eq. 1) then
!
        call getvtx('  ', 'ORTHO', 1, iarg, 8,&
                    ortho, ibid)
!
        if (ortho .eq. 'OUI     ') then
            call getvid(' ', 'MATRICE', 1, iarg, 1,&
                        matras, n1)
        else
            goto 9999
        endif
    else
        call getvid('ORTHO_BASE', 'MATRICE', 1, iarg, 1,&
                    matras, n1)
    endif
!
    if (n1 .ne. 0) then
        call dismoi('F', 'NOM_NUME_DDL', matras, 'MATR_ASSE', ibid,&
                    numdda, ier)
        call mtdscr(matras)
        matr=matras
        call jeveuo(matr//'.&INT', 'E', imatra)
        call dismoi('F', 'NOM_NUME_DDL', matras, 'MATR_ASSE', ibid,&
                    numdda, ier)
    else
        matr=' '
    endif
!
!----------------------------------------------------------------------
! --- RECUPERATION DES MODES PROPRES
!-----------------------------------------------------------------------
!
    if (ritz .eq. 1) then
        base=nomres
    else
        call getvid('ORTHO_BASE', 'BASE', 1, iarg, 1,&
                    base, n1)
    endif
!
! RECUPERATION DU TYPE ET DU NBRE DE MODES DES BASES
    call gettco(base, typbas)
    call rsorac(base, 'LONUTI', ibid, rbid, k8b,&
                cbid, rbid, 'ABSOLU', nbmode, 1,&
                ibid)
!
    call jeveuo(base//'           .ORDR', 'L', jordm)
! RECUPERATION DE LA NUMEROTATION DES BASES
    call jeveuo(base//'           .REFD', 'L', iadri1)
    if ((typbas.eq.'MODE_MECA') .or. (typbas.eq.'MODE_GENE')) then
        matri1 = zk24(iadri1)
    else
        matri1 = zk24(iadri1+2)
    endif
    if (matri1 .ne. ' ') then
        call dismoi('F', 'NOM_NUME_DDL', matri1, 'MATR_ASSE', ibid,&
                    numdd1, ier)
    else
        numdd1 = zk24(iadri1+3)(1:14)
    endif
!
    intf=zk24(iadri1+4)(1:8)
!
    if (numdd1 .ne. numdda) then
        call u2mess('I', 'ALGELINE2_81')
    endif
    nu = numdda(1:14)
    call jeveuo(nu//'.NUME.DEEQ', 'L', iddeeq)
    call jeveuo(nu//'.NUME.NEQU', 'L', llnequ)
    neq = zi(llnequ)
    call wkvect('&&ORTH99.BASE', 'V V R', nbmode*neq, idmode)
    call copmod(base, 'DEPL', neq, nu, nbmode,&
                'R', zr(idmode), cbid)
!-- FINALEMENT SI, DONC RECOPIE OK
!
!-----------------------------------------------------------------------
    call wkvect('&&ORTH99.TRAV1', 'V V R', neq, jtrav1)
    call wkvect('&&ORTH99.TRAV3', 'V V R', nbmode, jtrav3)
    call wkvect('&&ORTH99.TRAV4', 'V V I', neq, jtrav4)
!
    do 50 i = 1, neq
        zi(jtrav4+i-1) = 1
50  end do
!
    if (matr .eq. ' ') then
! ORTHONORMALISATION L2
        call vpgskp(neq, nbmode, zr(idmode), alpha, imatra,&
                    0, zr(jtrav1), zi(jtrav4), zr(jtrav3))
    else
! ORTHONORMALISATION PAR RAPPORT A LA MATRICE
        call vpgskp(neq, nbmode, zr(idmode), alpha, imatra,&
                    2, zr(jtrav1), zi(jtrav4), zr(jtrav3))
    endif
! MISE A ZEROS DES VECTEURS NON INDEPENDANTS
    call vecind(matr, idmode, neq, nbmode, 0,&
                nindep)
!
!-- GESTION DES CONCEPTS REENTRANTS
    call jeexin(nomres//'           .DESC', ier)
    if (ier .ne. 0) then
        call wkvect('&&ORTH99.VECT_TEMP', 'V V I', nbmode, ibid)
        do 10 i = 1, nbmode
            zi(ibid+i-1)=zi(jordm+i-1)
10      continue
        jordm=ibid
        call jedetc('G', nomres, 1)
    endif
    call rscrsd('G', nomres, 'MODE_MECA', nbmode)
!
!-- CREATION DU REFD POUR SD_VERI, ET REUTILISATION ULTERIEURE
    call jeexin(nomres(1:8)//'           .REFD', ibid)
    if (ibid .eq. 0) then
        call wkvect(nomres//'           .REFD', 'G V K24', 7, lrefe)
        zk24(lrefe)=' '
        zk24(lrefe+1)=' '
        zk24(lrefe+2)=' '
        zk24(lrefe+3)=numdd1
        zk24(lrefe+4)=intf
        zk24(lrefe+5)=' '
        zk24(lrefe+6)='RITZ'
        call jelibe(nomres//'           .REFD')
    endif
!
!
    iorne =0
    do 80 i = 1, nbmode
        iorol = zi(jordm+i-1)
        iorne = iorne+1
!
        call rsexch(' ', nomres, 'DEPL', iorne, chamol,&
                    ier)
        call vtcrem(chamol, matras, 'G', 'R')
        call jeveuo(chamol//'.VALE', 'E', jvale)
        do 111 ieq = 1, neq
            zr(jvale+ieq-1) = zr(idmode+(i-1)*neq+ieq-1)
111      continue
        call rsnoch(nomres, 'DEPL', iorne)
!
        call rsadpa(base, 'L', 1, 'NUME_MODE', iorol,&
                    0, iad, k8b)
        call rsadpa(nomres, 'E', 1, 'NUME_MODE', iorne,&
                    0, jiad, k8b)
        zi(jiad) = zi(iad)
!
        call rsadpa(base, 'L', 1, 'FREQ', iorol,&
                    0, iad, k8b)
        call rsadpa(nomres, 'E', 1, 'FREQ', iorne,&
                    0, jiad, k8b)
        zr(jiad) = zr(iad)
!
        call rsadpa(base, 'L', 1, 'NORME', iorol,&
                    0, iad, k8b)
        call rsadpa(nomres, 'E', 1, 'NORME', iorne,&
                    0, jiad, k8b)
        zk24(jiad) = zk24(iad)
!
        call rsadpa(base, 'L', 1, 'OMEGA2', iorol,&
                    0, iad, k8b)
        call rsadpa(nomres, 'E', 1, 'OMEGA2', iorne,&
                    0, jiad, k8b)
        zr(jiad) = zr(iad)
!
        call rsadpa(base, 'L', 1, 'MASS_GENE', iorol,&
                    0, iad, k8b)
        call rsadpa(nomres, 'E', 1, 'MASS_GENE', iorne,&
                    0, jiad, k8b)
        zr(jiad) = zr(iad)
!
        call rsadpa(base, 'L', 1, 'RIGI_GENE', iorol,&
                    0, iad, k8b)
        call rsadpa(nomres, 'E', 1, 'RIGI_GENE', iorne,&
                    0, jiad, k8b)
        zr(jiad) = zr(iad)
!
        call rsadpa(base, 'L', 1, 'TYPE_MODE', iorol,&
                    0, iad, k8b)
        call rsadpa(nomres, 'E', 1, 'TYPE_MODE', iorne,&
                    0, jiad, k8b)
        zk16(jiad) = zk16(iad)
80  end do
!
!
    call jedetr('&&ORTH99.TRAV1')
    call jedetr('&&ORTH99.TRAV3')
    call jedetr('&&ORTH99.TRAV4')
    call jedetr('&&ORTH99.BASE')
    call jedetr('&&ORTH99.VECT_TEM')
!
!
9999  continue
!
!      CALL JELIBE(NOMRES//'           .REFD')
!
!
    call jedema()
end subroutine
