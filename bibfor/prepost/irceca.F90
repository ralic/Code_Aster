subroutine irceca(ifi, ligrel, nbgrel, longr, ncmpmx,&
                  vale, nomgd, ncmpgd, celd, nbnoma,&
                  typma, nomsym, nbmat, lresu, nbcput,&
                  ncmput, imodl, ncmpv, nucmpv, nive)
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/dgmode.h"
#include "asterfort/digdel.h"
#include "asterfort/exisdg.h"
#include "asterfort/gicoor.h"
#include "asterfort/irmac2.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxlgut.h"
#include "asterfort/lxliis.h"
#include "asterfort/nbec.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: ifi, ligrel(*), nbgrel, longr(*), ncmpmx, celd(*)
    integer :: nbnoma(*), typma(*), nbmat, nbcput, imodl
    integer :: ncmpv, nucmpv(*), nive
    character(len=*) :: nomgd, ncmpgd(*), nomsym, ncmput(*)
    real(kind=8) :: vale(*)
    logical :: lresu
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
!        ECRITURE D'UN CHAMELEM SUR FICHIER CASTEM
!        A VALEURS REELLES
!  ENTREE:
!     IFI   : UNITE LOGIQUE DU FICHIER UNIVERSEL
!     LIGREL: LIGREL COMPLET
!     NBGREL: NOMBRE DE GRELS
!     LONGR : POINTEUR DE LONGUEUR DE LIGREL
!     NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR NOMGD
!     VALE  : VALEURS DU CHAM_ELEM
!     NOMGD : NOM DE LA GRANDEUR: SIEF_R, EPSI_R,...
!     NCMPGD: NOMS DES CMP
!     CELD  : DESCRIPTEUR DU CHAM_ELEM (MODES LOCAUX,ADRESSES->.CELV)
!     NBNOMA: NOMBRE DE NOEUDS DE CHAQUE MAILLE
!     TYPMA : TYPE_MAILLES
!     NBMAT : NOMBRE DE MAILLES A IMPRIMER
!     LRESU : =.TRUE. IMPRESSION D'UN CONCEPT RESULTAT
!     NBCPUT: NOMBRE DE CMP DEMANDE PAR L'UTILISATEUR
!     NCMPUT: NOMS DES CMP DEMANDE PAR L'UTILISATEUR
!     NIVE  : NIVEAU IMPRESSION CASTEM 3 OU 10
    integer :: nbvar, iad, itype, izero, iun
    integer :: modsav, nbelt, tabec(10)
    integer :: imodel, ilong
    character(len=3) :: toto
    character(len=24) :: valk(2)
    character(len=8) :: nomco, gtype, ktype, k8b
    character(len=16) :: ctype
    logical :: lmode, first, lnocen
!     ------------------------------------------------------------------
!
!  --- INITIALISATIONS ----
!
!-----------------------------------------------------------------------
    integer :: i, iachml, iacorr, ibid, ic, icm, icmc
    integer :: icmcas, icmp, icoef, icoma2, icomax, ideu, iel
    integer :: ielg, ielt, igr, igre, igrel, ij, inos
    integer :: inum, iobj, ipoin1, ipoin2, iret, iso, isp
    integer :: ispv, ityca, iutil, ivari, j, jadr
    integer ::  jlast, jli,  jmod
    integer ::  jv,  jvale, lkname, mode
    integer :: nbelgr, nbgr, nbsmo, nbsobj, nbva, ncmp, ncmpp
    integer :: nec, nnoe, npcalc, nsca, nscal
    integer, pointer :: bid(:) => null()
    integer, pointer :: entete(:) => null()
    logical, pointer :: jlogi(:) => null()
    integer, pointer :: jnbva(:) => null()
    integer, pointer :: nbrcmp(:) => null()
    character(len=8), pointer :: nomvar(:) => null()
    integer, pointer :: posvar(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
!
    if (.not.lresu) then
        call jeveuo('&&OP0039.LAST', 'E', jlast)
        inum = zi(jlast-1+5) + 1
    else
        inum = 0
    endif
    nbvar = 0
    izero = 0
!
    AS_ALLOCATE(vi=nbrcmp, size=nbgrel)
    AS_ALLOCATE(vi=entete, size=nbgrel*7)
    call jeveuo(jexnum('&&OP0039.LIGREL', imodl), 'E', jli)
    call jeveuo('&CATA.TE.MODELOC', 'L', imodel)
    call jeveuo(jexatr('&CATA.TE.MODELOC', 'LONCUM'), 'L', ilong)
!     ------------------------------------------------------------------
!
!     --- DETERMINATION DU NOMBRE MAXIMUM DE SOUS-POINTS ---
!
    icomax = 0
    nbsobj = 0
    nbsmo = zi(jli-1+1)
    AS_ALLOCATE(vl=jlogi, size=nbsmo*ncmpmx)
    AS_ALLOCATE(vi=jnbva, size=nbsmo+1)
    do 10 iso = 1, nbsmo
        lmode = .false.
        modsav = celd(celd(4+zi(jli+(iso-1)*(4+nbgrel)+5))+2)
        nbgr = zi(jli+(iso-1)*(4+nbgrel)+3)
        do 12 igr = 1, nbgr
            igre = zi(jli+(iso-1)*(4+nbgrel)+4+igr)
            icoef=max(1,celd(4))
            if (icoef .gt. icomax) icomax = icoef
12      continue
        if (ncmpv .gt. 0) then
            ncmp = 0
            do 14 i = 1, ncmpv
                if (nucmpv(i) .le. icomax) then
                    ncmp = ncmp + 1
                else if (nomgd.eq.'VARI_R') then
                    ncmp = ncmp + 1
                else
                    call codent(nucmpv(i), 'G', k8b)
                    nomco = 'V'//k8b
                    call utmess('A', 'PREPOST_74', sk=nomco)
                endif
14          continue
            if (ncmp .eq. 0) then
                call utmess('A', 'PREPOST_75')
                goto 9999
            endif
            icomax = ncmp
        endif
        if (icomax .gt. 999) then
            call utmess('F', 'PREPOST_76')
        endif
        do 16 igr = 1, nbgr
            igre = zi(jli+(iso-1)*(4+nbgrel)+4+igr)
            icoef=max(1,celd(4))
            mode=celd(celd(4+igre)+2)
            if (mode .eq. 0) goto 16
            modsav = mode
            if (mode .ne. modsav .and. mode .ne. 0) then
                call utmess('A', 'PREPOST_77')
                goto 10
            endif
            lmode = .true.
            call jeveuo(jexnum('&CATA.TE.MODELOC', mode), 'L', jmod)
            nec = nbec (zi(jmod-1+2))
            ASSERT(nec .le. 10)
            call dgmode(mode, imodel, ilong, nec, tabec)
            if (nbcput .ne. 0) then
                do 18 icm = 1, nbcput
                    if (nomgd .eq. 'VARI_R') then
                        call lxliis(ncmput(icm)(2:8), ivari, iret)
                        if ((ncmput(icm)(1:1).ne.'V') .or. (iret.ne.0)) then
                            valk (1) = ncmput(icm)
                            valk (2) = 'VARI_R'
                            call utmess('F', 'CALCULEL6_49', nk=2, valk=valk)
                        endif
                        jlogi((iso-1)*ncmpmx+ivari) = .true.
                        nbvar = nbvar + 1
                        goto 18
                    else
                        do 20 i = 1, ncmpmx
                            if (ncmput(icm) .eq. ncmpgd(i)) then
                                jlogi((iso-1)*ncmpmx+i) = .true.
                                nbvar = nbvar + 1
                                goto 18
                            endif
20                      continue
                    endif
                    valk (1) = ncmput(icm)
                    valk (2) = nomgd
                    call utmess('A', 'PREPOST5_25', nk=2, valk=valk)
18              continue
            else
                do 22 i = 1, ncmpmx
                    if (exisdg(tabec,i)) then
                        jlogi((iso-1)*ncmpmx+i) = .true.
                        nbvar = nbvar + 1
                    endif
22              continue
            endif
            if (nbvar .eq. 0) then
                call utmess('A', 'PREPOST_75')
                goto 9999
            endif
16      continue
        if (lmode) then
            nbsobj = nbsobj + 1
            nbrcmp(nbsobj) = digdel(modsav)
            entete((nbsobj-1)*7+1) = zi(jli+(iso-1)*(4+nbgrel)+2)
        else
            zi(jli+(iso-1)*(4+nbgrel)+3) = 0
        endif
10  end do
    jnbva(1) = icomax
!     ------------------------------------------------------------------
!
!     --- NOMS DES COMPOSANTES ET POSITIONS DANS LA GRANDEUR ---
!
    AS_ALLOCATE(vk8=nomvar, size=ncmpmx*icomax*nbsmo)
    AS_ALLOCATE(vi=posvar, size=ncmpmx*icomax*nbsmo)
    icoma2 = 0
    do 50 iso = 1, nbsmo
        nbgr = zi(jli+(iso-1)*(4+nbgrel)+3)
        do 58 igr = 1, nbgr
            igre = zi(jli+(iso-1)*(4+nbgrel)+4+igr)
            icoef=max(1,celd(4))
            if (icoef .gt. icoma2) icoma2 = icoef
58      continue
        do 52 i = 1, ncmpmx
            if (jlogi((iso-1)*ncmpmx+i)) then
                nomco = ncmpgd(i)
                if (nomco .eq. 'SIXX    ') then
                    nomco = 'SMXX    '
                else if (nomco.eq.'SIYY    ') then
                    nomco = 'SMYY    '
                else if (nomco.eq.'SIZZ    ') then
                    nomco = 'SMZZ    '
                else if (nomco.eq.'SIXY    ') then
                    nomco = 'SMXY    '
                else if (nomco.eq.'SIXZ    ') then
                    nomco = 'SMXZ    '
                else if (nomco.eq.'SIYZ    ') then
                    nomco = 'SMYZ    '
                endif
                jnbva(1+(iso-1)+1) = jnbva(1+(iso-1)+1) + 1
                nbva = jnbva(1+(iso-1)+1)
                if (icoma2 .gt. 1) then
                    if (ncmpv .gt. 0) then
                        do 54 isp = 1, ncmpv
                            call codent(nucmpv(isp), 'G', toto)
                            nomvar((iso-1)*ncmpmx*icomax+nbva-1+&
                            isp) = 'V'//toto
                            posvar((iso-1)*ncmpmx*icomax+nbva-1+&
                            isp) = i
54                      continue
                    else
                        do 56 isp = 1, icoma2
                            call codent(isp, 'G', toto)
                            nomvar((iso-1)*ncmpmx*icomax+nbva-1+&
                            isp) = 'V'//toto
                            posvar((iso-1)*ncmpmx*icomax+nbva-1+&
                            isp) = i
56                      continue
                    endif
                else
                    iutil= lxlgut(nomco)
                    if (iutil .le. 4) then
                        nomvar((iso-1)*ncmpmx*icomax+nbva) =&
                        nomco
                    else
                        nomvar((iso-1)*ncmpmx*icomax+nbva) =&
                        nomco(1:2)//nomco((iutil-1):iutil)
                    endif
                    posvar((iso-1)*ncmpmx*icomax+nbva) = i
                endif
            endif
52      continue
50  end do
!     ------------------------------------------------------------------
!
!     --- ECRITURE DE L'EN-TETE ---
!
    itype = 39
    izero = 0
    ideu = 2
    iun = 1
    write (ifi,'(A,I4)')   ' ENREGISTREMENT DE TYPE',ideu
    if (lresu) then
        if (nive .eq. 3) then
            write (ifi,'(A,I4,A,I4,A,I4)') ' PILE NUMERO',itype,&
            'NBRE OBJETS NOMMES ',izero,'NBRE OBJETS ',iun
        else if (nive.eq.10) then
            write (ifi,'(A,I4,A,I8,A,I8)') ' PILE NUMERO',itype,&
            'NBRE OBJETS NOMMES',izero,'NBRE OBJET ',iun
        endif
    else
        if (nive .eq. 3) then
            write (ifi,'(A,I4,A,I4,A,I4)') ' PILE NUMERO',itype,&
            'NBRE OBJETS NOMMES ',iun,'NBRE OBJETS ',iun
            write(ifi,'(1X,A8)') nomsym
            write(ifi,'(I5)') inum
        else if (nive.eq.10) then
            write (ifi,'(A,I4,A,I8,A,I8)') ' PILE NUMERO',itype,&
            'NBRE OBJETS NOMMES',iun,'NBRE OBJETS',iun
            write(ifi,'(1X,A8)') nomsym
            write(ifi,'(I8)') inum
        endif
    endif
!
    lkname = 16
    ibid = 4
    if (nive .eq. 3) write(ifi,'(16(I5))') nbsobj,ideu,ibid,lkname
    if (nive .eq. 10) write(ifi,'(10(I8))') nbsobj,ideu,ibid,lkname
    if (nomgd(1:4) .eq. 'SIEF' .or. nomgd(1:4) .eq. 'SIGM') then
        ctype = 'CONTRAINTES'
    else if (nomgd(1:4).eq.'EPSI') then
        ctype = 'DEFORMATIONS'
    else
        ctype = nomgd
    endif
    write(ifi,'(1X,A71)')  ctype
    iobj = 0
    do 100 iso = 1, nbsmo
        nbvar = jnbva(1+(iso-1)+1)
        icomax= jnbva(1)
        if (nbvar .ne. 0) then
            iobj = iobj + 1
            entete((iobj-1)*7+2) = izero
            entete((iobj-1)*7+3) = nbvar*icomax
            entete((iobj-1)*7+4) = izero
            entete((iobj-1)*7+5) = izero
            entete((iobj-1)*7+6) = izero
            entete((iobj-1)*7+7) = izero
        endif
100  end do
    if (nive .eq. 3) then
        write(ifi,'(16I5)') (entete(i),i=1,nbsobj*7)
    else if (nive.eq.10) then
        write(ifi,'(10I8)') (entete(i),i=1,nbsobj*7)
!      NOMS DES CONSTITUANTS (2A8 COLLES POUR UN K16 PAR SOUS-ZONE)
!      GIBI LIT DONC 2*NBSOBJ A8 SUR UN FORMAT 8(1X,A8), IL FAUT DONC
!      ECRIRE (2*NBSOBJ-1)/8+1 LIGNES
        do 101 iobj = 1, (2*nbsobj-1)/8+1
            write(ifi,'(A)')
101      continue
    endif
! 8001 FORMAT(8(1X,A8))
!     ------------------------------------------------------------------
!
!     --- IMPRESSION ---
!
    AS_ALLOCATE(vi=bid, size=ncmpmx*icomax)
    nbmat = longr(nbgrel+1)
    call gicoor()
    nbsmo = zi(jli-1+1)
    first = .true.
    icoma2 = 0
    lnocen=.false.
    do 200 iso = 1, nbsmo
        first = .true.
        nbgr = zi(jli+(iso-1)*(4+nbgrel)+3)
        nbvar = jnbva(1+(iso-1)+1)
        if (nbgr .ne. 0) then
            nbelt = zi(jli+(iso-1)*(4+nbgrel)+4)
            ielt = 0
            do 201 igr = 1, nbgr
                igre = zi(jli+(iso-1)*(4+nbgrel)+4+igr)
                icoef=max(1,celd(4))
                if (icoef .gt. icoma2) icoma2 = icoef
201          continue
            do 202 igr = 1, nbgr
                igrel = zi(jli+(iso-1)*(4+nbgrel)+4+igr)
                mode=celd(celd(4+igrel)+2)
                ipoin1 = longr(igrel)
                ipoin2 = longr(igrel+1)
                nbelgr = ipoin2-ipoin1-1
                if (mode .eq. 0) then
                    ielt = ielt + nbelgr
                    goto 202
                endif
                jmod = imodel+zi(ilong-1+mode)-1
                nec = nbec (zi(jmod-1+2))
                ASSERT(nec .le. 10)
                call dgmode(mode, imodel, ilong, nec, tabec)
                iad=celd(celd(4+igrel)+8)
                nscal = digdel(mode)
                icoef=max(1,celd(4))
                nsca = nscal*icoef
                ncmpp=0
                do 204 i = 1, ncmpmx
                    if (exisdg(tabec,i)) then
                        ncmpp = ncmpp+1
                    endif
204              continue
!
                iel = ligrel(ipoin1)
                itype = typma(iel)
                call jenuno(jexnum('&CATA.TM.NOMTM', itype), ktype)
!
                npcalc = nscal / ncmpp
                if (ktype .eq. 'QUAD9' .or. ktype .eq. 'TRIA7') npcalc = npcalc-1
                if (ktype .eq. 'PENTA18') npcalc = npcalc-3
                if (ktype .eq. 'SEG4') npcalc = npcalc-2
!
                if (first) then
                    call wkvect('&&IRCECA.VALE', 'V V R', nbelt*npcalc* nbvar*icomax, jvale)
                    first=.false.
                endif
!
! -- ECRITURE DE L'EN-TETE DE CHAQUE SOUS OBJETS ----
!
                call irmac2(ktype, ityca, gtype, ibid)
                call jeveuo(jexnom('&&GILIRE.CORR_ASTER_GIBI', gtype), 'L', iacorr)
                do 206 ielg = 1, nbelgr
                    iel = ligrel(ipoin1+ielg-1)
                    if (iel .le. 0) goto 206
                    ielt = ielt + 1
!
! --  RECHERCHE DE L'ADRESSE DANS VALE DU DEBUT DES VALEURS --
!
                    iachml = iad + nsca * (ielg-1)
!
!    --- CHAMELEM AUX NOEUDS ---
!
                    nnoe = nbnoma(iel)
                    if (ktype .eq. 'QUAD9' .or. ktype .eq. 'TRIA7') then
                        nnoe = nnoe-1
                        lnocen=.true.
                    endif
                    if (ktype .eq. 'SEG4') nnoe = nnoe-2
                    if (ktype .eq. 'PENTA18') then
                        nnoe = nnoe-3
                        lnocen=.true.
                    endif
                    if (npcalc .ne. nnoe) then
                        call utmess('F', 'PREPOST_79')
                    endif
                    itype = typma(iel)
                    do 214 inos = 1, nnoe
                        ij = zi(iacorr-1+inos)
                        j = iachml-1+ncmpp*icoma2*(ij-1)
                        jadr = jvale-1+(ielt-1)*nbvar*npcalc*icomax +(inos-1)*nbvar*icomax
                        ic = 0
                        do 208 icmp = 1, ncmpmx
                            if (exisdg(tabec,icmp)) then
                                ic = ic + 1
                                do 210 icmc = 1, nbvar
                                    icmcas = posvar((iso-1)*ncmpmx* icomax+icmc )
                                    if (icmp .eq. icmcas) then
                                        if (ncmpv .gt. 0) then
                                            do 211 ispv = 1, ncmpv
                                                if (nucmpv(ispv) .le. icoma2) zr(jadr+icmc-1+ispv&
                                                                              &)=vale(j+ ic-1+nuc&
                                                                              &mpv(ispv))
211                                          continue
                                        else
                                            do 212 isp = 1, icoma2
                                                zr(jadr+icmc-1+isp)=vale(j+ic-&
                                        1+isp)
212                                          continue
                                        endif
                                    endif
210                              continue
                            endif
208                      continue
214                  continue
206              continue
202          continue
            do 220 i = 1, nbvar*icomax
                bid(i) = izero
220          continue
            if (nive .eq. 3) then
                write(ifi,'(16I5)') (bid(i),i=1,nbvar*icomax)
            else if (nive.eq.10) then
                write(ifi,'(10I8)') (bid(i),i=1,nbvar*icomax)
            endif
            write(ifi,'(8(1X,A8))') (nomvar((iso-1)*ncmpmx*icomax+&
            i), i=1,nbvar*icomax)
            write(ifi,'(8(1X,A8))') ('REAL*8  ',' ',i=1,nbvar*icomax)
            bid(1) = npcalc
            bid(2) = nbelt
            bid(3) = izero
            bid(4) = izero
            do 222 jv = 1, nbvar
                do 224 isp = 1, icomax
                    if (nive .eq. 3) then
                        write(ifi,'(16I5)') (bid(i),i=1,4)
                    else if (nive.eq.10) then
                        write(ifi,'(10I8)') (bid(i),i=1,4)
                    endif
                    write(ifi,'(1P,3E22.13E3)') (zr(jvale-1+i),&
                    i=jv*isp,nbelt*npcalc*nbvar*icomax,nbvar*icomax)
224              continue
222          continue
            call jedetr('&&IRCECA.VALE')
        endif
200  end do
    if (lnocen) then
        call utmess('A', 'PREPOST_80')
    endif
!     ------------------------------------------------------------------
9999  continue
    if (.not.lresu) zi(jlast-1+5) = inum
    call jedetr('&&GILIRE.CORR_ASTER_GIBI')
    AS_DEALLOCATE(vi=bid)
    AS_DEALLOCATE(vi=entete)
    AS_DEALLOCATE(vl=jlogi)
    AS_DEALLOCATE(vi=jnbva)
    AS_DEALLOCATE(vi=nbrcmp)
    AS_DEALLOCATE(vk8=nomvar)
    AS_DEALLOCATE(vi=posvar)
    call jedetr('&&IRCECA.VALE')
!
    call jedema()
end subroutine
