subroutine rctrac(jmat, ktrac, nomcl, temp, jprol,&
                  jvale, nbvale, e)
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
! ----------------------------------------------------------------------
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
    integer :: imate, jprol, jvale, nbvale, jmat, nbmat
    real(kind=8) :: temp, e
    integer :: ktrac
    character(len=*) :: nomcl
! ----------------------------------------------------------------------
!     DETERMINATION DU MODULE DE YOUNG ET DE LA FONCTION D'ECROUISSAGE
!     A PARTIR DE LA COURBE DE TRACTION D'UN MATERIAU DONNE
!
! IN  IMATE  : ADRESSE DU MATERIAU CODE
! IN  KTRAC  : 1 -> 'TRACTION'
!              2 -> 'META_TRACTION'
!              3 -> 'META_TRAC_ZIRC'
! IN  NOMCL  : NOM DU MOT CLE POUR LA COURBE DE TRACTION
!              SI KTRAC = 1 NOMCL = 'SIGM'
!              SI KTRAC = 2
!               NOMCL = 'SIGM_F' OU 'SIGM_B' OU 'SIGM_M' OU 'SIGM_A'
! IN  TEMP   : TEMPERATURE AU POINT DE GAUSS CONSIDERE
! IN  COMP   :
! IN  MOT    :
! OUT JPROL  : ADRESSE DE L'OBJET .PROL DE LA S.D. FONCTION R(P)
! OUT JVALE  : ADRESSE DE L'OBJET .VALE DE LA S.D. FONCTION R(P)
! OUT NBVALE : NOMBRE DE VALEURS DE LA FONCTION R(P)
! OUT E      : MODULE DE YOUNG
!
!
!
!
    aster_logical :: procon, ltrac
    integer :: icomp, ipi, idf, nbf, ivalk, ik, ipif, ipifc, jpro
    integer :: jvalf1, nbvf1, k, k1, k2, nar
    integer :: jvaln, nbvn, i, j, jvalf2, nbvf2
    real(kind=8) :: coef, tole, t1, t2, e1, e2, z1, z2, zp1, zp2
    real(kind=8) :: rprim1, rp1, rprim2, rp2
    character(len=24) :: valk(2)
    character(len=1) :: pro1, pro2
! ----------------------------------------------------------------------
! PARAMETER ASSOCIE AU MATERIAU CODE
    integer :: lmat, lfct, lsup
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    parameter(lmat=7,lfct=9,lsup=2)
! DEB ------------------------------------------------------------------
!      TOLE=R8PREM() TROP PETIT
    tole=1.d-6
!
    nbmat=zi(jmat)
!     UTILISABLE SEULEMENT AVEC UN MATERIAU PAR MAILLE
    ASSERT(nbmat.eq.1)
    imate=jmat+zi(jmat+nbmat+1)
!
! - COURBE DE TRACTION SANS METALLURGIE
    if (ktrac .eq. 1) then
        do 10 icomp = 1, zi(imate+1)
            if ('TRACTION' .eq. zk32(zi(imate)+icomp-1)(1:8)) then
                ipi=zi(imate+2+icomp-1)
                goto 20
            endif
 10     continue
        call utmess('F', 'MODELISA6_80')
 20     continue
        idf=zi(ipi)+zi(ipi+1)
        nbf=zi(ipi+2)
        ivalk=zi(ipi+3)
        do 30 ik = 1, nbf
            if ('SIGM    ' .eq. zk8(ivalk+idf+ik-1)) then
                ipif=ipi+lmat-1+lfct*(ik-1)
                goto 40
            endif
 30     continue
        ASSERT(.false.)
 40     continue
    endif
!
! - COURBE DE TRACTION AVEC METALLURGIE
!
    if (ktrac .eq. 2) then
        do 50 icomp = 1, zi(imate+1)
            if ('META_TRACTION' .eq. zk32(zi(imate)+icomp-1)(1:13)) then
                ipi=zi(imate+2+icomp-1)
                goto 60
            endif
 50     continue
        call utmess('F', 'MODELISA6_82')
 60     continue
!
        idf=zi(ipi)+zi(ipi+1)
        nbf=zi(ipi+2)
        ivalk=zi(ipi+3)
!
        if (nomcl(1:7) .eq. 'SIGM_F1') then
            do 70 ik = 1, nbf
                if ('SIGM_F1  ' .eq. zk8(ivalk+idf+ik-1)) then
                    ipif=ipi+lmat-1+lfct*(ik-1)+lsup*(ik-1)
                    goto 80
                endif
 70         continue
            call utmess('F', 'MODELISA6_83')
 80         continue
        endif
!
        if (nomcl(1:7) .eq. 'SIGM_F2') then
            do 90 ik = 1, nbf
                if ('SIGM_F2  ' .eq. zk8(ivalk+idf+ik-1)) then
                    ipif=ipi+lmat-1+lfct*(ik-1)+lsup*(ik-1)
                    goto 100
                endif
 90         continue
            call utmess('F', 'MODELISA6_84')
100         continue
        endif
!
        if (nomcl(1:7) .eq. 'SIGM_F3') then
            do 110 ik = 1, nbf
                if ('SIGM_F3  ' .eq. zk8(ivalk+idf+ik-1)) then
                    ipif=ipi+lmat-1+lfct*(ik-1)+lsup*(ik-1)
                    goto 120
                endif
110         continue
            call utmess('F', 'MODELISA6_85')
120         continue
        endif
!
        if (nomcl(1:7) .eq. 'SIGM_F4') then
            do 130 ik = 1, nbf
                if ('SIGM_F4  ' .eq. zk8(ivalk+idf+ik-1)) then
                    ipif=ipi+lmat-1+lfct*(ik-1)+lsup*(ik-1)
                    goto 140
                endif
130         continue
            call utmess('F', 'MODELISA6_86')
140         continue
        endif
!
        if (nomcl(1:6) .eq. 'SIGM_C') then
            do 150 ik = 1, nbf
                if ('SIGM_C  ' .eq. zk8(ivalk+idf+ik-1)) then
                    ipif=ipi+lmat-1+lfct*(ik-1)+lsup*(ik-1)
                    goto 160
                endif
150         continue
            call utmess('F', 'MODELISA6_87')
160         continue
        endif
    endif
!
    ipifc=zi(ipif+6)
    jprol=zi(ipifc)
    jvale=zi(ipifc+1)
!
!
! LES POINTEURS JPROL ET JVALE SUR ZONE DE TRAVAIL RDPE
! ETENDU SONT MODIFIES PAR RCTRAC
!
    jpro=zi(ipif+1)
!
    if (zk24(jpro)(1:1) .eq. 'C') then
!
! ----- FONCTION CONSTANTE - IMPOSSIBLE
!
        call utmess('F', 'MODELISA6_88', sk=nomcl)
    else if (zk24(jpro)(1:1).eq.'F') then
!
! ----- FONCTION D'UNE SEULE VARIABLE
!
        jvalf1=zi(ipif+2)
        nbvf1=zi(ipif)
        nbvale=nbvf1
!
        do 170 k = 1, nbvf1
            zr(jvale+k-1)=zr(jvalf1+k-1)
            zr(jvale+k-1+nbvf1)=zr(jvalf1+k-1+nbvf1)
170     continue
        zk24(jprol+4)=zk24(jpro+4)
        zk24(jprol+5)=zk24(jpro+5)
!
    else if (zk24(jpro)(1:1).eq.'N') then
!
! ----- NAPPE : FONCTION DE DEUX VARIABLES, DETERMINATION DE I TEL QUE
! ----- ZR(JVALN+I) < TEMP < ZR(JVALN+I+1)
!
        jvaln=zi(ipif+4)
        nbvn=zi(ipif+5)
        zk24(jprol+5)=zk24(jpro+5)
        procon=.false.
        if (temp .lt. zr(jvaln)) then
            if (zk24(jpro+4)(1:1) .eq. 'C') then
                i=1
                procon=.true.
            else if (zk24(jpro+4)(1:1).eq.'L') then
                i=1
            else if (zk24(jpro+4)(1:1).eq.'E') then
                call utmess('F', 'MODELISA6_89', sk=nomcl)
            endif
        else if (temp.gt.zr(jvaln+nbvn-1)) then
            if (zk24(jpro+4)(2:2) .eq. 'C') then
                i=nbvn
                procon=.true.
            else if (zk24(jpro+4)(2:2).eq.'L') then
                i=nbvn-1
            else if (zk24(jpro+4)(2:2).eq.'E') then
                call utmess('F', 'MODELISA6_90', sk=nomcl)
            endif
        else
            do 180 j = 1, nbvn-1
!          IF (ZR(JVALN+J-1) .LE.TEMP .AND. TEMP .LE. ZR(JVALN+J)) THEN
!          CAS DE STRICTE EGALITE. PAS D'INTERPOLATION
                t1=zr(jvaln+j-1)
                t2=zr(jvaln+j)
                if (abs(t1-temp) .le. (tole*abs(t1))) then
                    i=j
                    procon=.true.
                    goto 190
                endif
                if (abs(t2-temp) .le. (tole*abs(t2))) then
                    i=j+1
                    procon=.true.
                    goto 190
                endif
                if (t1 .lt. temp .and. temp .lt. t2) then
                    i=j
                    goto 190
                endif
180         continue
190         continue
        endif
!
! ----- INTERPOLATION ENTRE I ET I+1
!
        jvalf1=zi(ipif+2)+zi(zi(ipif+3)+i-1)-1
        nbvf1=zi(zi(ipif+3)+i)-zi(zi(ipif+3)+i-1)
        nbvf1=nbvf1/2
        if (procon) then
!
! ------- SI LE PROLONGEMENT EST CONSTANT, ON SE RAMENE AU CAS FONCTION
!
            do 200 k = 1, nbvf1
                zr(jvale+k-1)=zr(jvalf1+k-1)
                zr(jvale+k-1+nbvf1)=zr(jvalf1+k-1+nbvf1)
200         continue
            zk24(jprol+4)=zk24(jpro+6+2*i)
            nbvf2=nbvf1
            nbvale=nbvf1
        else
!
! ------- INTERPOLATION POUR LA FONCTION ENTRE I ET I+1
!
            zk24(jprol+4)(1:2)='CC'
            if (zk24(jpro+6+2*i)(1:1) .eq. 'E' .or. zk24(jpro+6+2*i+2)( 1:1) .eq. 'E') then
                zk24(jprol+4)(1:1)='E'
                elseif (zk24(jpro+6+2*i)(1:1).eq.'L' .or. zk24(jpro+6+2*i+&
            2)(1:1).eq.'L') then
                zk24(jprol+4)(1:1)='L'
            endif
            if (zk24(jpro+6+2*i)(2:2) .eq. 'E' .or. zk24(jpro+6+2*i+2)( 2:2) .eq. 'E') then
                zk24(jprol+4)(2:2)='E'
                elseif (zk24(jpro+6+2*i)(2:2).eq.'L' .or. zk24(jpro+6+2*i+&
            2)(2:2).eq.'L') then
                zk24(jprol+4)(2:2)='L'
            endif
            jvalf2=zi(ipif+2)+zi(zi(ipif+3)+i)-1
            nbvf2=zi(zi(ipif+3)+i+1)-zi(zi(ipif+3)+i)
            nbvf2=nbvf2/2
!
! ------- INTERPOLATION ENTRE LES COURBES R(P,T1) ET R(P,T2)
! ----------------------------------------------------------
!
!
! ------- INITIALISATION DES VARIABLES  ET INTERPOLATION POUR P=0
!
            nbvale=nbvf1+nbvf2-1
            coef=(temp-zr(jvaln+i-1))/(zr(jvaln+i)-zr(jvaln+i-1))
            ltrac=.false.
            if (ktrac .eq. 1) then
                e1=zr(jvalf1+nbvf1)/zr(jvalf1)
                e2=zr(jvalf2+nbvf2)/zr(jvalf2)
                e=e1+coef*(e2-e1)
                ltrac=.true.
                zr(jvale)=0.d0
                k1=1
                k2=1
            else
                nbvale=nbvale+1
                z1=zr(jvalf1)
                z2=zr(jvalf2)
                if (abs(z2-z1) .le. (tole*z1)) then
                    zr(jvale)=z1
                    k1=1
                    k2=1
                else if (z2.gt.z1) then
                    zr(jvale)=z1
                    k1=1
                    k2=0
                else
                    zr(jvale)=z2
                    k1=0
                    k2=1
                endif
            endif
!
            zr(jvale+nbvale)=zr(jvalf1+nbvf1)+ coef*(zr(jvalf2+nbvf2)-&
            zr(jvalf1+nbvf1))
!
            pro1=zk24(jpro+6+2*i)(2:2)
            pro2=zk24(jpro+6+2*i+2)(2:2)
            nar=0
!
! ------- DEBUT DE LA BOUCLE D'INTERPOLATION
! ------- LA LONGUEUR DE CETTE BOUCLE VAUT NBVF1+NBVF2-2
!
! ------- DANS LE <<CAS COURANT>>:
! ------- ON PROGRESSE POINT PAR POINT PAR ABSCISSES CROISSANTES,
! ------- AVEC 2 COMPTEURS K1 ET K2 (UN PAR COURBE)
! ------- LE CPTEUR NAR (NBRE A RETIRER) EST INCREMENTE DE 1 A CHAQUE
! ------- FOIS QUE 2 POINTS DES COURBES 1 ET 2 ONT LA MEME ABSCISSE
!
            do 220 k = 1, nbvale-1
                if ((k1.lt.nbvf1) .or. (k2.lt.nbvf2)) then
!
! ----------- CAS OU ON ARRIVE AU BOUT DE LA COURBE 1 :
! -----------   PROLONGEMENT EXCLUS => ON CALCULE NAR ET ON ARRETE
! -----------   SINON (L OU C) =>ON FAIT L'INTERPOLATION A L'ABSCISSE
! -----------                    Z2 ET ON INCREMENTE K2
!
                    if (k1 .ge. nbvf1) then
                        if (pro1 .eq. 'E') then
                            nar=nbvale-k
                            goto 230
!
                        else if (pro1.eq.'L') then
                            rprim1=(zr(jvalf1+nbvf1+k1-1)-zr(jvalf1+&
                            nbvf1+k1-2))/ (z1-zp1)
                        else
                            rprim1=0.d0
                        endif
                        z2=zr(jvalf2+k2)
                        if (ltrac) z2=z2-zr(jvalf2+nbvf2+k2)/e2
                        zr(jvale+k)=z2
                        rp1=zr(jvalf1+nbvf1+k1-1)+rprim1*(z2-z1)
                        rp2=zr(jvalf2+nbvf2+k2)
                        zr(jvale+nbvale+k)=rp1+coef*(rp2-rp1)
                        k2=k2+1
                        goto 210
!
                    else
                        zp1=zr(jvalf1+k1-1)
                        if (ltrac) zp1=zp1-zr(jvalf1+nbvf1+k1-1)/e1
                        z1=zr(jvalf1+k1)
                        if (ltrac) z1=z1-zr(jvalf1+nbvf1+k1)/e1
                    endif
!
! ----------- CAS OU ON ARRIVE AU BOUT DE LA COURBE 2 : IDEM
!
                    if (k2 .ge. nbvf2) then
                        if (pro2 .eq. 'E') then
                            nar=nbvale-k
                            goto 230
!
                        else if (pro2.eq.'L') then
                            rprim2=(zr(jvalf2+nbvf2+k2-1)-zr(jvalf2+&
                            nbvf2+k2-2))/ (z2-zp2)
                        else
                            rprim2=0.d0
                        endif
                        z1=zr(jvalf1+k1)
                        if (ltrac) z1=z1-zr(jvalf1+nbvf1+k1)/e1
                        zr(jvale+k)=z1
                        rp1=zr(jvalf1+nbvf1+k1)
                        rp2=zr(jvalf2+nbvf2+k2-1)+rprim2*(z1-z2)
                        zr(jvale+nbvale+k)=rp1+coef*(rp2-rp1)
                        k1=k1+1
                        goto 210
!
                    else
                        zp2=zr(jvalf2+k2-1)
                        if (ltrac) zp2=zp2-zr(jvalf2+nbvf2+k2-1)/e2
                        z2=zr(jvalf2+k2)
                        if (ltrac) z2=z2-zr(jvalf2+nbvf2+k2)/e2
                    endif
!
! ----------- <<CAS COURANT>> : CF COMMENTAIRE AU DEBUT DE LA BOUCLE
!
                    if (abs(z2-z1) .le. (tole*z1)) then
                        zr(jvale+k)=z1
                        rp1=zr(jvalf1+nbvf1+k1)
                        rp2=zr(jvalf2+nbvf2+k2)
                        nar=nar+1
                        k1=k1+1
                        k2=k2+1
                    else
                        if (z2 .gt. z1) then
                            zr(jvale+k)=z1
                            rp1=zr(jvalf1+nbvf1+k1)
                            rprim2=(zr(jvalf2+nbvf2+k2)-zr(jvalf2+&
                            nbvf2+k2-1))/ (z2-zp2)
                            rp2=zr(jvalf2+nbvf2+k2-1)+rprim2*(z1-zp2)
                            k1=k1+1
                        else
                            zr(jvale+k)=z2
                            rp2=zr(jvalf2+nbvf2+k2)
                            rprim1=(zr(jvalf1+nbvf1+k1)-zr(jvalf1+&
                            nbvf1+k1-1))/ (z1-zp1)
                            rp1=zr(jvalf1+nbvf1+k1-1)+rprim1*(z2-zp1)
                            k2=k2+1
                        endif
                    endif
                    zr(jvale+nbvale+k)=rp1+coef*(rp2-rp1)
                endif
210             continue
220         continue
!
! ------- FIN DE LA BOUCLE D'INTERPOLATION
!
230         continue
!
! ------- CORRECTION DE NBVALE ET
! ------- DECALAGE DES ORDONNEES DE NAR VERS LA GAUCHE
!
            if (nar .gt. 0) then
                nbvale=nbvale-nar
                do 240 k = 1, nbvale
                    zr(jvale+nbvale+k-1)=zr(jvale+nbvale+nar+k-1)
240             continue
            endif
        endif
    else
        valk(1)=zk24(jpro)
        valk(2)=nomcl
        call utmess('F', 'MODELISA6_91', nk=2, valk=valk)
    endif
!
! --- CONSTRUCTION DE LA COURBE R(P) POUR TRACTION
! --- DANS LE CAS DE LA FONCTION D'UNE SEULE VARIABLE
! --- PAS LA PEINE EN METALLURGIE CAR ON DONNE DIRECTEMENT
! --- LA COURBE R(P) OU PLUS EXACTEMENT LA COURBE R(R)
!
    if (ktrac .eq. 1) then
        if (zk24(jpro)(1:1) .eq. 'N') then
            if (.not.(procon)) goto 260
        else
            if (zk24(jpro)(1:1) .ne. 'F') goto 260
        endif
        e=zr(jvale+nbvale)/zr(jvale)
        zr(jvale)=0.d0
        do 250 k = 1, nbvale-1
            zr(jvale+k)=zr(jvale+k)-zr(jvale+nbvale+k)/e
250     continue
260     continue
    endif
!
end subroutine
