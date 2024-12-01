*22  select
        s.name as software_name,
        count(distinct d.ds_id) as use_count,
        count(distinct d.ds_id) / (select count(distinct d1.ds_id)
                                                                from dataset d1, anal
yzes a1
                                                                where d1.ds_id = a1.d
s_id) as proportion
     from dataset d, analyzes a, software s, keywords k, subjects su
     where d.ds_id = a.ds_id
        and a.sw_id = s.sw_id
         and d.ds_id = k.ds_id
         and d.ds_id = su.ds_id
     group by s.name
     order by use_count desc;
