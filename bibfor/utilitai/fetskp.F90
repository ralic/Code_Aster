subroutine fetskp()
!
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
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:
!       - CREATION DU GRAPHE D'ENTREE DU PARTITIONNEUR
!       - GERE LES POIDS SUR LES MAILLES
!       - GERE LE GROUPAGE DES MAILLES
!       - APPEL A METIS OU EXECUTION DE SCOTCH
!       - CREATION DE NOUVEAUX GROUPES DE MAILLES
!       - VERIFICATION DE LA CONNEXITE DES SOUS-DOMAINES
!----------------------------------------------------------------------
! person_in_charge: aimery.assire at edf.fr
!
! CORPS DU PROGRAMME
! aslint: disable=W1303,W1501
    implicit none
!
!
! DECLARATION VARIABLES LOCALES
#include "jeveux.h"
!
#include "asterc/aplext.h"
#include "asterc/fetsco.h"
#include "asterc/getfac.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvtx.h"
#include "asterc/gtoptk.h"
#include "asterfort/creaco.h"
#include "asterfort/creagm.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxcadr.h"
#include "asterfort/lxlgut.h"
#include "asterfort/mpicm0.h"
#include "asterfort/mpicm2.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mess.h"
#include "asterfort/ulnume.h"
#include "asterfort/ulopen.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/verico.h"
#include "asterfort/wkvect.h"
    integer :: nbmama, idco, temp, sdb, nbmato, mail, renum2, nbma, nomsdm, masd
    integer :: nbmasd, idmasd, id, isd, renum1, err, co, renum, nbre, nbmabo
    integer :: mail2, velo, edlo, poids, numsdm, nmap, i, j, ima, nbbord, lrep
    integer :: iulm1, iocc, nocc, ifm, niv, nblien, nbpart, renum3, idma, iulm2
    integer :: mabord, val, rang, nbproc, versco, n1, n2, n3, ier, iaux, iaux2
    integer :: vali(2), iret
    real(kind=8) :: tmps(6)
    character(len=8) :: ma, k8bid, ktmp, mod, verif, ktmp2, meth, bord, k8nb
    character(len=8) :: kersco
    character(len=24) :: k24b, grpema, nom, sdbord
    character(len=256) :: jnom(4)
    character(len=128) :: rep
    integer :: iarg
!
! CORPS DU PROGRAMME
    call jemarq()
    call infniv(ifm, niv)
!
! ------- ON RECUPERE LE NBRE DE PROCS ET LE RANG
    nbproc=1
    rang=0
    call mpicm0(rang, nbproc)
!
! ********************************************************************
!                       CREATION DU GRAPHE
!
! ------- ON RECUPERE LES DONNEES DU MAILLAGE OU DU MODELE
!
    call getvid(' ', 'MAILLAGE', 0, iarg, 1,&
                ma, err)
    if (err .eq. 0) then
        call u2mess('F', 'UTILITAI_78')
    endif
    call dismoi('F', 'NB_MA_MAILLA', ma, 'MAILLAGE', nbmato,&
                k8bid, err)
    if (err .ne. 0) then
        call u2mess('F', 'UTILITAI_79')
    endif
    call wkvect('&&FETSKP.RENUM1', 'V V I', nbmato, renum1)
    call getvid(' ', 'MODELE', 0, iarg, 1,&
                mod, err)
    if (err .eq. 0) then
        write(ifm,*)' -- AUCUN MODELE PRIS EN COMPTE'
        call wkvect('&&FETSKP.RENUM', 'V V I', nbmato, renum)
        do 90 ima = 1, nbmato
            zi(renum-1+ima)=ima
            zi(renum1-1+ima)=ima
90      continue
    else
        write(ifm,*)' -- PRISE EN COMPTE DU MODELE :',mod
        nbmato=0
        call jelira(mod//'.MODELE    .LIEL', 'NMAXOC', nocc, k8bid)
        do 94 iocc = 1, nocc
            call jelira(jexnum(mod//'.MODELE    .LIEL', iocc), 'LONMAX', nbma, k8bid)
            nbmato=nbmato+nbma-1
94      continue
        call wkvect('&&FETSKP.RENUM', 'V V I', nbmato, renum)
        id=1
        do 93 iocc = 1, nocc
            call jelira(jexnum(mod//'.MODELE    .LIEL', iocc), 'LONMAX', nbma, k8bid)
            call jeveuo(jexnum(mod//'.MODELE    .LIEL', iocc), 'L', idma)
            do 91 ima = 1, nbma-1
                zi(renum-1+id)=zi(idma-1+ima)
! --------- ON VERIFIE QUE LE MODELE NE CONTIENT PAS DE MAILLES TARDIVES
! --------- QUI SONT ESSENTIELLEMENT DES NOEUDS A CE STADE
                if (zi(idma-1+ima) .lt. 0) then
                    call u2mess('F', 'FETI0_3')
                endif
                zi(renum1-1+zi(idma-1+ima))=id
                id=id+1
91          continue
93      continue
    endif
!
! ------- CREATION DE LA CONNECTIVITE DES MAILLES
!
    call creaco(nbmato, ma, bord, nbbord, nblien,&
                nbmabo)
!
! ------ ON RECUPERE LES TABLEAUX CONSTRUITS DANS CREACO
!
    call jeveuo('&&FETSKP.RENUM2', 'L', renum2)
    call jeveuo('&&FETSKP.RENUM3', 'L', renum3)
    call jeveuo('&&FETSKP.CO', 'L', co)
    call jeveuo('&&FETSKP.IDCO', 'L', idco)
    call jeveuo('&&FETSKP.NBMAMA', 'L', nbmama)
    call jeveuo('&&FETSKP.MABORD', 'E', mabord)
!
! ------- ON RECUPERE LE NBRE DE SD ET LE PARTITONNEUR
!
    call getvis(' ', 'NB_PART', 0, iarg, 1,&
                nbpart, err)
    call getvtx(' ', 'METHODE', 0, iarg, 1,&
                meth, err)
!
! -------  UTILISATION DE CONTRAINTES
!
    call wkvect('&&FETSKP.VELO', 'V V S', nbmato, velo)
    call wkvect('&&FETSKP.EDLO', 'V V S', nblien, edlo)
    do 114 ima = 1, nbmato
        zi4(velo-1+ima)=1
114  end do
    do 115 i = 1, nblien
        zi4(edlo-1+i)=1
115  end do
!
    call getfac('GROUPAGE', nocc)
    do 116 iocc = 1, nocc
        call getvtx('GROUPAGE', 'GROUP_MA', iocc, iarg, 1,&
                    grpema, err)
        call jelira(ma//'.GROUPEMA', 'NMAXOC', nbre, k8bid)
        nbma=0
        do 118 j = 1, nbre
            call jenuno(jexnum(ma//'.GROUPEMA', j), nom)
            if (nom .eq. grpema) then
                call jelira(jexnum(ma//'.GROUPEMA', j), 'LONUTI', nbma, k8bid)
                call jeveuo(jexnum(ma//'.GROUPEMA', j), 'L', idma)
            endif
118      continue
        if (nbma .eq. 0) then
            call u2mess('F', 'UTILITAI_80')
        endif
        write(ifm,*)'  - GROUPAGE  :',grpema
        write(ifm,*)' '
        do 204 ima = 1, nbma-1
            mail=zi(renum3-1+zi(renum1-1+zi(idma-1+ima)))
            do 212 i = ima+1, nbma
                mail2=zi(renum3-1+zi(renum1-1+zi(idma-1+i)))
                do 205 j = zi4(idco-1+mail), zi4(idco-1+mail+1)-1
                    if (zi4(co-1+j) .eq. mail2) zi4(edlo-1+j)=nbmato+ 1
205              continue
                do 220 j = zi4(idco-1+mail2), zi4(idco-1+mail2+1)-1
                    if (zi4(co-1+j) .eq. mail) zi4(edlo-1+j)=nbmato+1
220              continue
212          continue
204      continue
116  end do
!
    call getfac('POIDS_MAILLES', nocc)
    do 117 iocc = 1, nocc
        call getvtx('POIDS_MAILLES', 'GROUP_MA', iocc, iarg, 1,&
                    grpema, err)
        call jelira(ma//'.GROUPEMA', 'NMAXOC', nbre, k8bid)
        nbma=0
        do 207 j = 1, nbre
            call jenuno(jexnum(ma//'.GROUPEMA', j), nom)
            if (nom .eq. grpema) then
                call jelira(jexnum(ma//'.GROUPEMA', j), 'LONUTI', nbma, k8bid)
                call jeveuo(jexnum(ma//'.GROUPEMA', j), 'L', idma)
            endif
207      continue
        if (nbma .eq. 0) then
            call u2mess('F', 'UTILITAI_80')
        endif
        call getvis('POIDS_MAILLES', 'POIDS', iocc, iarg, 1,&
                    poids, err)
        write(ifm,*)'  - POIDS_MAILLES :',grpema
        write(ifm,*)'       AVEC UN POIDS DE : ',poids
        write(ifm,*) ' '
        do 203 ima = 1, nbma
            mail=zi(renum3-1+zi(renum1-1+zi(idma-1+ima)))
            zi4(velo-1+mail)=poids
203      continue
117  end do
!
! ------- ON IMPRIME LE GRAPH SI PROC 0
!
    if ((meth(1:6).ne.'SCOTCH') .and. (rang.eq.0)) then
        iulm1 = ulnume ()
        if (iulm1 .eq. -1) call u2mess('F', 'UTILITAI_81')
        call ulopen(iulm1, ' ', ' ', 'NEW', 'O')
        write(iulm1,'(I12,I12,I3)')nbmato,nblien/2,11
!
!         DO 70 IMA=1,NBMATO
!           NBFORM=0
!           ITMP=2*( 1+ZI4(IDCO-1+IMA+1)-1-ZI4(IDCO-1+IMA) ) + 2
!           IF (ITMP.GT.NBFORM) NBFORM=ITMP
!  70     CONTINUE
!         WRITE(K8NB,'(''('',I4,''I8'','')'')') NBFORM
!
        do 71 ima = 1, nbmato
            write(k8nb,'(''('',I4,''I8'','')'')') 2*(1+zi4(idco-1+ima+&
            1)-1 - zi4(idco-1+ima) ) + 2
            write(iulm1,k8nb)zi4(velo-1+ima), (zi4(co-1+i),zi4(edlo-1+&
            i), i=zi4(idco-1+ima),zi4(idco-1+ima+1)-1)
71      continue
!
        call ulopen(-iulm1, ' ', ' ', ' ', ' ')
!
! CREATION DU FICHIER DU GRAPHE POUR L'APPEL EXTERNE DE SCOTCH
!      ELSE
!
!        IULM1 = ULNUME ()
!        CALL ULOPEN ( IULM1,' ', ' ', 'NEW', 'O' )
!        WRITE(IULM1,'(I1)')0
!        WRITE(IULM1,'(I12,I12)')NBMATO,NBLIEN
!        WRITE(IULM1,'(I1,I2,I2)')1,0,11
!        DO 153 IMA=1,NBMATO
!          WRITE(IULM1,'(500I8)')ZI4(VELO-1+IMA),ZI(NBMAMA-1+IMA),
!     &                         (ZI4(EDLO-1+I),ZI4(CO-1+I),
!     &                         I=ZI4(IDCO-1+IMA),ZI4(IDCO-1+IMA+1)-1)
! 153    CONTINUE
! FIN CREATION DU FICHIER DU GRAPHE POUR SCOTCH
!
    endif
!
    call jedetr('&&FETSKP.RENUM1')
    call jedetr('&&FETSKP.NBMAMA')
!
!
! ********************************************************************
!                       LANCEMENT DU LOGICIEL
!
!
!     ************** LANCEMENT DE SCOTCH
!
    if (meth(1:6) .eq. 'SCOTCH') call wkvect('&&FETSKP.NMAP', 'V V S', nbmato, nmap)
    if ((meth(1:6).eq.'SCOTCH') .and. (rang.eq.0)) then
! CETTE PARTIE PERMET L'APPEL EXTERNE DE SCOTCH - LAISSER EN L'ETAT
!        CALL GETVTX(' ','LOGICIEL' ,0,IARG,1,REP,ERR)
!        IF ( ERR .NE. 0 ) THEN
!          IULM3 = ULNUME ()
!          CALL ULOPEN ( IULM3,' ', ' ', 'NEW', 'O' )
!          WRITE(KTMP,'(I4)') NBPART
!          CALL LXCADR(KTMP)
!          WRITE(IULM3,'(a,a)')'cmplt '//KTMP
!          WRITE(KTMP2,'(I4)') IULM1
!          CALL LXCADR(KTMP2)
!          WRITE(KTMP3,'(I4)') IULM3
!          CALL LXCADR(KTMP3)
!          IULM2 = ULNUME ()
!          CALL ULOPEN ( IULM2,' ', ' ', 'NEW', 'O' )
!          WRITE(KTMP4,'(I4)') IULM2
!          CALL LXCADR(KTMP4)
!          LREP=0
!          DO 277 I=1,LEN(REP)
!            IF (REP(I:I).NE.' ') LREP=LREP+1
! 277      CONTINUE
!          JNOM(1)=REP(1:LREP)
!          JNOM(2)='fort.'//KTMP2
!          JNOM(3)='fort.'//KTMP3
!          JNOM(4)='fort.'//KTMP4
!          CALL ULOPEN (-IULM1,' ',' ',' ',' ')
!          CALL ULOPEN (-IULM3,' ',' ',' ',' ')
!          CALL APLEXT(NIV,4,JNOM,ERR)
!        ELSE
! FIN APPEL EXTERNE DE SCOTCH
!
!         APPEL DE SCOTCH PAR LIBRAIRIE (PASSAGE PAR FETSCO.C)
        if (niv .ge. 2) then
            call uttcpu('CPU.FETSKP', 'INIT', ' ')
            call uttcpu('CPU.FETSKP', 'DEBUT', ' ')
        endif
        write(ifm,*) ' '
        write(ifm,*) '***************** SCOTCH *****************'
        write(ifm,*) ' '
        write(ifm,*) ' '
        write(ifm,*) ' * LE NOMBRE DE MAILLES    :',nbmato
        write(ifm,*) ' * LE NOMBRE DE CONNEXIONS :',nblien
        write(ifm,*) ' '
        call fetsco(nbmato, nblien, zi4(co), zi4(idco), nbpart,&
                    zi4(nmap), zi4(edlo), zi4(velo), versco, ier)
        n1=versco/10000
        n2=(versco-n1*10000)/100
        n3=versco-n1*10000-n2*100
        kersco(1:8)='........'
        write(kersco(1:2),'(I2)')n1
        write(kersco(4:5),'(I2)')n2
        write(kersco(7:8),'(I2)')n3
        if (niv .ge. 2) then
            call uttcpu('CPU.FETSKP', 'FIN', ' ')
            call uttcpr('CPU.FETSKP', 6, tmps)
            write(ifm,*) ' * TEMPS DE PARTITIONNEMENT  :',tmps(3)
            write(ifm,*) ' '
        endif
        write(ifm,*) '********** FIN SCOTCH ',kersco,' *********'
        if (ier .ne. 0) call u2mesi('F', 'UTILITAI_56', 1, ier)
        write(ifm,*) ' '
!      ENDIF
!
!     ************** LANCEMENT DE METIS
!
    else if (rang.eq.0) then
        write(ktmp,'(I4)') nbpart
        write(ktmp2,'(I4)') iulm1
        call lxcadr(ktmp)
        call lxcadr(ktmp2)
        jnom(2)='fort.'//ktmp2
        jnom(3)=ktmp
        call getvtx(' ', 'LOGICIEL', 0, iarg, 1,&
                    rep, err)
        if (err .eq. 0) then
            call gtoptk('repout', rep, iret)
            if (iret .ne. 0) then
                vali(1) = len(rep)
                call u2mesi('F', 'EXECLOGICIEL0_24', 1, vali)
            endif
            lrep = lxlgut(rep)
            if (meth .eq. 'PMETIS  ') then
                jnom(1)=rep(1:lrep)//'/pmetis'
            else if (meth .eq. 'KMETIS  ') then
                jnom(1)=rep(1:lrep)//'/kmetis'
            endif
        else
            lrep=0
            do 77 i = 1, len(rep)
                if (rep(i:i) .ne. ' ') lrep=lrep+1
77          continue
            jnom(1)=rep(1:lrep)
        endif
        call aplext(niv, 3, jnom, err)
    endif
!
    call jedetr('&&FETSKP.EDLO')
    call jedetr('&&FETSKP.VELO')
!
!
! ********************************************************************
!                    CREATION DES GROUPES DE MAILLES
!
    sdb=0
    if (bord .eq. 'OUI     ') then
        call getvtx('        ', 'NOM_GROUP_MA_BORD', 0, iarg, 1,&
                    sdbord, err)
        if (err .ne. 0) then
            nbpart=2*nbpart
            sdb=1
        endif
    endif
!
    call wkvect('&&FETSKP.NUMSDM', 'V V I', nbmabo, numsdm)
    call wkvect('&&FETSKP.NBMASD', 'V V I', nbpart, nbmasd)
!
! ------- LECTURE DU RESULTAT DU PARTITONNEUR
!
    if (meth(1:6) .ne. 'SCOTCH') then
        if (rang .eq. 0) then
            iulm2 = ulnume ()
            if (iulm2 .eq. -1) call u2mess('F', 'UTILITAI_81')
            lrep=0
            do 177 i = 1, len(ktmp2)
                if (ktmp2(i:i) .ne. ' ') lrep=lrep+1
177          continue
            jnom(1)='fort.'//ktmp2(1:lrep)//'.part.'//ktmp
            call ulopen(iulm2, jnom(1), ' ', 'OLD', 'O')
            do 35 ima = 1, nbmato
                read(iulm2,'(I4)')zi(numsdm-1+zi(renum2-1+ima))
35          continue
            call ulopen(-iulm2, ' ', ' ', ' ', ' ')
        endif
        k24b='&&FETSKP.NUMSDM'
        call mpicm2('BCAST', k24b)
        do 36 ima = 1, nbmato
            iaux=zi(renum2-1+ima)
            iaux2=zi(numsdm-1+iaux)
            zi(nbmasd+iaux2)=zi(nbmasd+iaux2)+1
36      continue
    else
! CETTE PARTIE PERMET L'APPEL EXTERNE DE SCOTCH - LAISSER EN L'ETAT
!        CALL ULOPEN (-IULM2,' ',' ',' ',' ')
!        CALL ULOPEN ( IULM2,'fort.97',' ', 'OLD', 'O' )
!        CALL GETVTX('        ','LOGICIEL' ,0,IARG,1,REP,ERR)
!        IF ( ERR .NE. 0 ) THEN
!         READ(IULM2,*)NBRE
!         DO 165 IMA=1,NBMATO
!           READ(IULM2,*)NBRE,ZI(NUMSDM-1+ZI(RENUM2-1+IMA))
!           ZI(NBMASD+ZI(NUMSDM-1+ZI(RENUM2-1+IMA)))=
!     &                     ZI(NBMASD+ZI(NUMSDM-1+ZI(RENUM2-1+IMA)))+1
! 165     CONTINUE
!          CALL ULOPEN (-IULM2,' ',' ',' ',' ')
!        ELSE
! FIN APPEL EXTERNE DE SCOTCH
        k24b='&&FETSKP.NMAP'
        call mpicm2('BCAST', k24b)
        do 135 ima = 1, nbmato
            zi(numsdm-1+zi(renum2-1+ima))=zi4(nmap-1+ima)
            zi(nbmasd+zi(numsdm-1+zi(renum2-1+ima)))= zi(nbmasd+zi(&
            numsdm-1+zi(renum2-1+ima)))+1
135      continue
        call jedetr(k24b)
!        ENDIF
    endif
!
! ------- ON REMET LES MAILLES DE BORDS
!
    if (bord .eq. 'OUI     ') then
        if (sdb .eq. 0) then
            do 55 ima = 1, nbbord
                mail=zi(renum2-1+nbmato+ima)
                if (zi(mabord+zi(mabord-1+mail)-1) .ne. 0) then
                    zi(mabord-1+mail)=zi(mabord+zi(mabord-1+mail)-1)
                endif
!
                zi(numsdm-1+mail)=zi(numsdm-1+zi(mabord-1+mail))
                zi(nbmasd+zi(numsdm-1+zi(mabord-1+mail)))= zi(nbmasd+&
                zi(numsdm-1+zi(mabord-1+mail)))+1
55          continue
        else
            do 45 ima = 1, nbbord
                mail=zi(renum2-1+nbmato+ima)
                if (zi(mabord+zi(mabord-1+mail)-1) .ne. 0) then
                    zi(mabord-1+mail)=zi(mabord+zi(mabord-1+mail)-1)
                endif
!
                zi(numsdm-1+mail)=zi(numsdm-1+zi(mabord-1+mail))+&
                nbpart/2
                zi(nbmasd+nbpart/2+zi(numsdm-1+zi(mabord-1+mail)))=&
                zi(nbmasd+nbpart/2+zi(numsdm-1+zi(mabord-1+mail)))+1
45          continue
        endif
    endif
!
! ------- VERIFICATION DE LA CONNEXITE
!
    call getvtx(' ', 'CORRECTION_CONNEX', 0, iarg, 1,&
                verif, err)
    val=0
    if (verif .eq. 'OUI     ') then
        call verico(nbmato, nbpart, val)
    endif
!
! ------- ON RECONSTRUIT NBMASD
!
    if (val .eq. 1) then
        call jedetr('&&FETSKP.NBMASD')
        call wkvect('&&FETSKP.NBMASD', 'V V I', nbpart, nbmasd)
        do 145 ima = 1, nbmabo
            zi(nbmasd+zi(numsdm-1+ima))=zi(nbmasd+zi(numsdm-1+ima))+1
145      continue
    endif
!
! ------- CREATION DES GROUP_MA
!
    nbmato=nbmabo
    call creagm(nbmato, nbpart, sdb, ma, sdbord,&
                masd)
!
! ------- ON RECUPERE LES TABLEAUX CREES DANS CREAGM
!
    call jeveuo('&&FETSKP.MASD', 'L', masd)
    call jeveuo('&&FETSKP.IDMASD', 'L', idmasd)
    call jeveuo('&&FETSKP.NOMSDM', 'L', nomsdm)
!
! ------- ON EFFECTUE LE VOISINAGE DE CHAQUE SD
!
    if (val .eq. 1) then
        call wkvect('&&FETSKP.TEMP', 'V V I', nbpart, temp)
        do 166 isd = 1, nbpart
            id=0
            do 167 ima = zi(idmasd-1+isd), zi(idmasd-1+isd+1)-1
                if (zi(mabord-1+zi(masd-1+ima)) .ne. 0) then
                    if (bord .eq. 'OUI     ') goto 167
                endif
                mail=zi(renum3-1+zi(masd-1+ima))
                do 168 i = zi4(idco-1+mail), zi4(idco-1+mail+1)-1
                    mail2=zi4(co-1+i)
                    if (zi(numsdm-1+zi(renum2-1+mail2)) .ne. (isd-1)) then
                        do 169 j = 1, id
                            if (zi(temp-1+j) .eq. zi(numsdm-1+zi(renum2- 1+mail2))) goto 170
169                      continue
                        zi(temp+id)=zi(numsdm-1+zi(renum2-1+mail2))
                        id=id+1
170                      continue
                    endif
168              continue
167          continue
            write(ifm,*)zk24(nomsdm-1+isd),' CONTIENT '&
     &                ,zi(nbmasd-1+isd),' MAILLES  ET SES VOISINS :'&
     &                ,(zk24(nomsdm+zi(temp-1+j)),j=1,id)
166      continue
    else
        do 56 i = 1, nbpart
            write(ifm,*)'LE SOUS DOMAINE ',zk24(nomsdm-1+i),' CONTIENT '&
     &                     ,zi(nbmasd-1+i),' MAILLES '
56      continue
        call jedetr('&&FETSKP.TEMP')
    endif
!
    call jedetr('&&FETSKP.RENUM2')
    call jedetr('&&FETSKP.RENUM3')
    call jedetr('&&FETSKP.IDMASD')
    call jedetr('&&FETSKP.NOMSDM')
    call jedetr('&&FETSKP.MASD')
    call jedetr('&&FETSKP.NBMASD')
    call jedetr('&&FETSKP.NUMSDM')
    call jedetr('&&FETSKP.RENUM')
    call jedetr('&&FETSKP.CO')
    call jedetr('&&FETSKP.IDCO')
    call jedetr('&&FETSKP.MABORD')
!
    call jedema()
end subroutine
